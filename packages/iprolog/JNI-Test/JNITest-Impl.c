#include <jni.h>
#include "JNITest.h"
#include <stdio.h>

JNIEXPORT void JNICALL
Java_JNITest_displayHelloWorld(JNIEnv *env, jobject obj) {
  
  printf("Hello world!\n");
  return;
}
