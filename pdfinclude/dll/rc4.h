// arc4.h

// __declspec(dllexport) is required for Win32.
#ifdef MAKE_UNIX
	#define DLLEXPORT
#else
	#define DLLEXPORT __declspec( dllexport )
#endif

typedef struct arc4_key
{      
     unsigned char state[256];       
     unsigned char x;        
     unsigned char y;
} arc4_key;

void prepare_key(unsigned char *key_data_ptr,int key_data_len,arc4_key *key);
void arc4(unsigned char *buffer_ptr,int buffer_len,arc4_key * key);

extern DLLEXPORT void _cdecl endecrypt(unsigned char *buffer_ptr,int buffer_len, unsigned char *pwd_ptr, int pwd_len);
