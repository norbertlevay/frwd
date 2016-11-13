
#include <errno.h>
#include <time.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include <linux/limits.h>

// to manipulate filenames
#include <libgen.h>


#include <openssl/evp.h>

#include <openssl/sha.h>
#include <openssl/hmac.h>
#include <openssl/bio.h>
#include <openssl/buffer.h>


//#include <bzlib.h>
//#include <zlib.h>


/*
/////////////////////////////////////////////////////

Notes on versions/standards related to decryption

Information needed to decrypt fwd files:
which encryption algorithm was used in which mode
and which padding was applied to the data. 
Additionally, which hashing function was used to 
generate the key and in how many iterations.

Also note that for fwd-file's header sentences the Base64
encoding was used. Must be decoded before decryption.
Measurement data is purely binary data after
the header (it is not Base64 encoded).

References:
[Oaks] Scott Oaks: "Java Security", Second Edition,
O'Reilly <copyright> 2001

[PKCS5] (pkcs-5.ps) PKCS#5: Password-Based Encryption Standard
An RSA Laboratories Technical Note
Version 1.5,
Revised November 1, 1993

[EVP] manual pages of EVP_BytesToKey() and EVP_get_cipherbyname()


/////////////////////////////////////////////////////

NOTES (algorithm, mode and padding):

-- In original FRWD.jar :

SecretKeyFactory.getProvider() returns: SunJCE version 1.7
(Sun Java Cryptographic Extension)

Algorithm: SecretKeyFactory and Cipher use instance of "PBAWithMD5AndDES"
which is defined by PKCS#5 [Oaks, p290 top].

Mode: Also in [Oaks, p290 bottom] CBC is the only mode which will
work for "PBEWithMD5AndDES".

Padding: Also in [Oaks, p292 bottom], algorithm passed to
getInstance function is in form: algorithm/mode/padding.
If only algorithm is specified ("DES") rest are taken as defaults.
For SunJCE provider DES dafaults are CBC and PKCS5 padding
"DES" = "DES/CBC/PKCS5Padding".

-- In linux:

Use OpenSSL's "EVP library" openssl/evp.h, a library
of high level cryptographic functions.

The manual pages 'man EVP_BytesToKey()' says:
if total key + IV length < digest_length and MD5 is
used then algorithm is compatible with PKCS#5 v1.5.

Tha man page gives algorithm:

D_i = HASH^count(D_(i-1) || data || salt)

compare to PKCS#5 p.5, "6.1 DES Key Generation" steps 1,2,3.

The man page for EVP_get_cipherbyname("des-cbc") in NOTE says:
PKCS padding works by adding n-bytes of value n to make data 
multiple of block size.

See padding in PKCS#5 p.5 "6.2 Encryption block formatting".
*/




/* --- fwd_ky_iv_generator()   ---------------------------

 this funct is crucial to decrypting fwd files with
 command line tools openssl enc -d -des-cbc

 this func will generate key & iv from fwd password and salt
 such that key and iv can be used with openssl enc -d des-cbc

 INPUT: password & salt from fwd
 OUTPUT: key & IV for DES-CBC as of openssl
	 and for decode License and Map params (see read_map())

 Must conform to key generation as of standard PKCS#5

*/
unsigned char key[EVP_MAX_KEY_LENGTH], iv[EVP_MAX_IV_LENGTH];

int fwd_key_iv_generator(void)
{
    const EVP_CIPHER *cipher;
    const EVP_MD *dgst = NULL;

	// the three values below password & salt & iteration
	// are from class Crypto of FRWD.jar printed
	// while running
    const char *password = "QwKrR0tmobc73Pl8Y";
    const unsigned char salt[] = {0xA8, 0x1B, 0x9C, 0x7A,\
			   	  0x60, 0x29, 0x38, 0x99};
    int i,iters = 20;


    OpenSSL_add_all_algorithms();

    cipher = EVP_get_cipherbyname("des-cbc");
    if(!cipher) { fprintf(stderr, "No des-cbc cipher. "); return 1; }

    dgst=EVP_get_digestbyname("md5");
    if(!dgst) { fprintf(stderr, "No md5 digest. "); return 1; }

    if(!EVP_BytesToKey(cipher, dgst, salt,
        (unsigned char *) password,
        strlen(password), iters, key, iv))
    {
        fprintf(stderr, "EVP_BytesToKey failed. ");
        return 1;
    }
    // THE USERS MUST NOT SEE THIS!!!
    //    printf("Iters : %d  and Key seems to miss parity, but decrypts anyway... \n",iters);
    //    printf("Key (%d) : ",cipher->key_len); for(i=0; i<cipher->key_len; ++i) { printf(" %02x", key[i]); } printf("\n");
    //    printf("IV  (%d) : ",cipher->iv_len);  for(i=0; i<cipher->iv_len; ++i) { printf(" %02x", iv[i]); } printf("\n");

    return 0;
}



char *unbase64(unsigned char *input, int length)
{
  BIO *b64, *bmem;

  char *buffer = (char *)malloc(length);
  memset(buffer, 0, length);

  b64 = BIO_new(BIO_f_base64());
  bmem = BIO_new_mem_buf(input, length);
  bmem = BIO_push(b64, bmem);

  BIO_read(bmem, buffer, length);

  BIO_free_all(bmem);

  return buffer;
}

/* example of ciphertext
  unsigned char ciphertext[]= {   0x66, 0xe4, 0xf3, 0xa1,
                            0x1f, 0x30, 0x65, 0xa1,
                             0xaf, 0x81, 0xe2, 0x9c,
                            0xf3, 0xb2, 0x9c, 0x29};
*/
int decrypt_buffer(unsigned char * PARAMciphertext, int ct_len)
{

    EVP_CIPHER_CTX de;
    EVP_CIPHER_CTX_init(&de);
    const EVP_CIPHER *cipher_type;
    unsigned char *mode;
    char plaintext[1024];
    int len, i = 0;
/* pick from program-globals filled in by
   fwd_key_iv_generator() above.

  unsigned char iv[]  = {  0x08, 0x83, 0x4c, 0xd3,
                           0xfd, 0x90, 0xf3, 0xf3};

  unsigned char key[]= {   0x90, 0x21, 0xc8, 0x29,
                           0x24, 0xac, 0xd8, 0x29};
*/
    unsigned char ciphertext[1024];

    // unbase64 first;  ciphertext has EOL
    //printf("Base64 ciphertext (%d)  %s",ct_len, PARAMciphertext);
    char *output = unbase64(PARAMciphertext, ct_len);
    //printf("Unbase64:  %s %d\n", output, strlen(output));
    len = strlen(output);
    memcpy(ciphertext, output, strlen(output));
    free(output);

    ciphertext[len] = 0;

    //printf("Initializing DES ALGORITHM FOR CBC MODE..\n");
    cipher_type = EVP_des_cbc();
    EVP_DecryptInit_ex(&de, cipher_type, NULL, key, iv);
    // NULL for ENGINE* will force default engine
    // ENGINE represents the actual implementation of the cipher (incl HW acclerated)
    int p_len = len;
    int f_len = 0;
    if(!EVP_DecryptInit_ex(&de, NULL, NULL, NULL, NULL)){
      fprintf(stderr, "Decryption library error. ");
      return -2;
    }
    EVP_CIPHER_CTX_set_padding(&de, 0);

    if(!EVP_DecryptUpdate(&de, plaintext, &p_len, ciphertext, len)){
      fprintf(stderr,"Decryption library error. ");
      return -1;
    }

    //printf("plaintext length value before DecryptFinal p_len = %d\n", p_len);

    if(!EVP_DecryptFinal_ex(&de, plaintext+p_len, &f_len)){
      fprintf(stderr,"Decryption library error. ");
      return -1;
    }

    EVP_CIPHER_CTX_cleanup(&de);

    //printf("final length after des_decrypt p_len + f_len = %d\n", p_len + f_len);
    plaintext[p_len+f_len] = 0;
    //printf("plaintext (%d): %s\n",p_len+f_len, plaintext);
    //printf("ciphertext    : %s\n", ciphertext);

    memcpy(PARAMciphertext,plaintext,p_len+f_len+1);

  return 0;
}

/* example of ciphertext
  unsigned char ciphertext[]= {   0x66, 0xe4, 0xf3, 0xa1,
                            0x1f, 0x30, 0x65, 0xa1,
                             0xaf, 0x81, 0xe2, 0x9c,
                            0xf3, 0xb2, 0x9c, 0x29};
*/

// read from f, write to fdat
int read_blocks_and_decrypt( FILE *f, FILE * fdat/*, unsigned long nbytes*/){

    #define BLOCK_SIZE  (512)
    static unsigned char block[BLOCK_SIZE];
    int rc,k;
    size_t nr;

  EVP_CIPHER_CTX de;
  EVP_CIPHER_CTX_init(&de);
  const EVP_CIPHER *cipher_type;
  unsigned char *mode;
  unsigned char plaintext[1024];
  unsigned char ciphertext[1024];
  int p_len = 0;


    cipher_type = EVP_des_cbc();
    EVP_DecryptInit_ex(&de, cipher_type, NULL, key, iv);

   if(!EVP_DecryptInit_ex(&de, NULL, NULL, NULL, NULL)){
       fprintf(stderr,"Decryption library error. ");
     //printf("ERROR in EVP_DecryptInit_ex \n");
     return 3;
   }
   //EVP_CIPHER_CTX_set_padding(&de, 0);

	while( !feof(f)) {

		nr = fread(block, sizeof(unsigned char) ,BLOCK_SIZE, f);
		if (ferror(f)) {
                    //printf("fread error %d\n",nr);
                    perror("fread");
			return 1;
		}

                if(!EVP_DecryptUpdate(&de, plaintext, &p_len, block, nr)){
                    fprintf(stderr,"Decryption error. ");
                  //printf("ERROR in EVP_DecryptUpdate\n");
                  return 2;
                }

                size_t nw = fwrite(plaintext, sizeof(unsigned char) ,p_len, fdat);
		if (ferror(f)) {
		    //    printf("fwrite error %d %d\n",nr,nw);
                    perror("fwrite");
			return 1;
		}

        }
      int f_len =0;
      if(!(rc=EVP_DecryptFinal_ex(&de, plaintext, &p_len))){
          //printf("ERROR in EVP_DecryptFinal_ex (%d)\n",rc);
          fprintf(stderr,"Decryption error. ");
         // ERR_print_errors_fp(stderr);
          return 2;
       }

      size_t nw = fwrite(plaintext, sizeof(unsigned char) ,p_len, fdat);
 	if (ferror(f)) {
// 		printf("fwrite error %d %d\n",nr,nw);
                    perror("fwrite");
 		return 1;
 	}

    EVP_CIPHER_CTX_cleanup(&de);

	return 0;
}

#if(0)
// was used to output pure encypted data, without headers
// read from f, write to fdat
int read_blocks( FILE *f, FILE * fdat/*, unsigned long nbytes*/){

    #define BLOCK_SIZE  (512)
    static unsigned char block[BLOCK_SIZE];
    int rc,k,i=0;
    

	while( !feof(f)) {

		size_t nr = fread(block, sizeof(unsigned char) ,BLOCK_SIZE, f);
		if (ferror(f)) {
			printf("fread error %d\n",nr);
			return 1;
		}

                size_t nw = fwrite(block, sizeof(unsigned char) ,nr, fdat);
		if (ferror(f)) {
			printf("fwrite error %d %d\n",nr,nw);
			return 1;
		}

	}
	return 0;
}
#endif

/* Base64 encoded License example
	unsigned char lic[] = \
	{0x39, 0x26, 0x10, 0x38, 0x03, 0x75, 0x77, 0x1d,  \
 	 0xc2, 0x6e, 0x28, 0xf0, 0xe7, 0x48, 0x60, 0x18,  \
 	 0x86, 0xa5, 0x3c, 0x61, 0x34, 0x16, 0xbb, 0x58};
*/
// Licnse is Base64 encoded.
// Read until EOL.
int read_line( FILE *f, int loff){

	char *line=NULL;
	int nl = 0;
	ssize_t rcn;

	if(-1==(rcn = getline(&line,&nl,f))){
            perror("getline");
	    exit(1);
	}

        //printf("(%d) .%s",strlen(line),line );
	//printf("\n%s (%d)\n",line+loff,strlen(line+loff));

        if(decrypt_buffer(line+loff, rcn-loff)){
            return -1;
        }

        //printf("(%d) .%s\n",strlen(line), line );

	if((line[0] == 'I') && (line[1]=='S')){
		// read out map size
		return atoi(line+3);
	}
	free(line);
	return 0;
}

int read_license_map( FILE *f){

	// Simplixtic version for now...
	// map parameters starts with M otherwise
	// are encoded like License so we reuse read_license
	int map_size = 0;
	fpos_t fpos;
	int rc = 0;
	fgetpos(f,&fpos);
	char c = fgetc(f);
	while((c == 'L') || (c == 'M')){
		rc = read_line(f,c=='L'?2:3);
		if(rc<0) return 1;
		if(rc>0) map_size = rc;
		fgetpos(f,&fpos);
		c = fgetc(f);
	}
        //if(map_size) printf("MAP Image Size %d bytes.\n",map_size);
	fsetpos(f,&fpos);
        if(fseek(f,map_size,SEEK_CUR)){
            perror("fseek");
            return 1;
        }

#if(0)
	fsetpos(f,&fpos);
	// if there was a map we need to read & write it out
	// map image size is in MIS_xxxxxx it needs 
	// size = atoi(xxx)
	static unsigned char map[10000000];
	size_t nr = fread(map, sizeof(unsigned char) ,map_size, f);
        if (ferror(f)) {
            perror("fread(map)");
		//printf("fread error %d\n",nr);
		return 1;
        }
#endif
	return 0;
}

int read_head( FILE *f){

	char *line=NULL;
	int nl = 0;
	ssize_t rcn;

        if(-1==(rcn = getline(&line,&nl,f))){
            perror("getline");
            return 1;
	}
        //printf("(%d) %s",strlen(line) ,line);
        free(line);
        return 0;
}
#if(0)
void print_date(){
	time_t now;
	char* nowstr;
	struct tm *tm_now;
	char bnow[1024];

	now = time(NULL);
	tm_now = localtime(&now);
	strftime(bnow,1024, "# %Y %m %d %H:%M:%S",tm_now);
	printf("%s",bnow);

}
int new_filename(char * path){

	char dname[4092];
	char fname[4092];

	strcpy(dname,path);//, strlen(path));
	strcpy(fname,path);//, strlen(path));
//	printf("%s : %s\n",__func__,path);
//	printf("%s dir  : %s\n",__func__,dirname(dname));
//	printf("%s file : %s\n",__func__,basename(fname));

        // Better: change extension to small letters and check for .fwd match only FIXME
	// this solution below skips .Fwd .fWD and similar...
	char * p = NULL;
	if( NULL == (p = strstr(fname,".fwd")) ){
		//printf("File does not have .fwd extension %s\n",fname);

		if(NULL == (p = strstr(fname,".FWD"))){
			printf("File does not have .FWD or .fwd extension %s\n",fname);
			return 0;
		}
	}
	p[0] = 0;
	strcat(p,".xml.zip");
	strcpy(path,basename(fname));
	return 0;
}
#endif


// Version 0.9 (and not 1.0) because error handling is missing
void print_usage(void){
    printf("fwd2xml version: 0.9.0\n");
//    printf("\n");
    printf("Usage: \n");
    printf("  fwd2xml somefile(s).fwd\n");
//    printf("  fwd2xml *.fwd\n");
    printf("\n");
    printf("Note: Creates zipped xml file(s).fwd.xml.zip\n");
    printf("      These FWRD zip-files contain a file always named 'measurement'.\n");
    printf("      To preserve the name of the zip-file itself,\n");
    printf("      unzip with -p option:\n");
    printf("\n");
    printf("  unzip -p mylogfile.fwd.xml.zip > mylogfile.fwd.xml\n");
    printf("\n");
    printf("Also note that fwd-file and zipped-xml file sizes are almost equal.\n");
    printf("However some fwd-files contain a map. Then file sizes may differ,\n");
    printf("because the map is not extracted.\n");
    printf("\n");
}

int main(int argc, char *argv[]) {

	int n=0,i,rc=0;
	char outfilename[PATH_MAX];

        if(argc == 1) {
            print_usage();
            return 0;
        }

	// fill in global key & iv
        if(fwd_key_iv_generator()){
            fprintf(stderr,"OpenSSl/evp error.\n");
            exit(1);
        }
	for(n=1; n<argc; n++) {

		//printf("next fname: %s\n", argv[n]);
		strcpy(outfilename, argv[n]);
                strcat(outfilename,".xml.zip");
                //printf("output fname: %s\n",outfilename);

		FILE *f = fopen(argv[n], "r");
		if (!f) {
                    perror("fopen(read)");
		    exit(1);
		}
		FILE *fdat = fopen(outfilename, "w");
		if (!fdat) {
                    perror("fopen(write)");
		    exit(1);
		}
		if( read_head(f) ) {
		    exit(1);
		}
		if( read_license_map(f) ) {
                        fprintf(stderr, "File damaged or not a fwd-file: %s\n",argv[n]);
		    exit(1);
		}
		if( rc = read_blocks_and_decrypt(f,fdat) ) {
                    if(rc==2)
                        fprintf(stderr, "File damaged or not a fwd-file: %s\n",argv[n]);
                    exit(1);
		}
		fclose(fdat);
		fclose(f);

                //printf("\n");
	}
	return 0;
}

