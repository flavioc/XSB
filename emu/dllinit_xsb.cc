#include <windows.h>
#include <stdio.h>
extern "C"
{
  int WINAPI win_xsbent (HANDLE h, DWORD reason, void *ptr);
};
int WINAPI win_xsbent(HANDLE hinst, DWORD reason, LPVOID reserved)
{
  switch (reason)
    {
    case DLL_PROCESS_ATTACH:
      break;
    case DLL_PROCESS_DETACH:
      break;
    case DLL_THREAD_ATTACH:
      break;
    case DLL_THREAD_DETACH:
      break;
    }
  return 1;
}

