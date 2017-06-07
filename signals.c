#include <sys/wait.h>

//gcc -shared -o libsignals.so -fPIC signals.c

int childExited(int status) 
{
    return WIFEXITED(status);
}

int childStopped(int status)
{
    return WIFSTOPPED(status);
}