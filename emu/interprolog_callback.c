/* 
** Author(s): Miguel Calejo, Vera Pereira
** Contact:   interprolog@declarativa.com, http://www.declarativa.com, http://www.xsb.com
** Copyright (C) XSB Inc., USA, 2001
** Use and distribution, without any warranties, under the terms of the 
** GNU Library General Public License, readable in http://www.fsf.org/copyleft/lgpl.html
*/

/* This file provides the implementation for the native methods of NativeEngine.java, 
as well as the XSB Prolog built-in used by InterProlog */
#include <jni.h>
#include "com_0002fxsb_0002finterprolog_0002fNativeEngine.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <time.h>
// for interrupts:
#include "sig_xsb.h"
#include <signal.h>
// extern int *asynint_ptr;
extern void keyint_proc(int);

/* The following include is necessary to get the macros and routine
   headers */

#include "cinterf.h"


JNIEnv *theEnv;
jobject theObj;
jboolean debug;

JNIEXPORT jbyteArray JNICALL 
Java_com_xsb_interprolog_NativeEngine_get_1bytes
(JNIEnv *env, jobject obj) {
		int i = 1;
		jbyteArray bytes;
		jsize size = p2p_arg(reg_term(1), 3);
		int newHead, newTail;
		jbyte *b;
		if (size == -1) {
			xsb_close_query();
			return NULL;
		}
		b = (jbyte *) malloc(size);
		newHead = p2p_car(p2p_arg(reg_term(1), 2));
		newTail = p2p_cdr(p2p_arg(reg_term(1), 2));
		b[i-1] = p2c_int(newHead);
		while(is_nil(newTail) == 0) { 
			newHead = p2p_car(newTail);
			b[i] = p2c_int(newHead);
			newTail = p2p_cdr(newTail);
			i++;
		}
		xsb_close_query();
		bytes = (*env)->NewByteArray(env, size);
		(*env)->SetByteArrayRegion(env, bytes, 0, size, b);
		free(b);
		return bytes;
}

JNIEXPORT jint JNICALL 
Java_com_xsb_interprolog_NativeEngine_put_1bytes
(JNIEnv *env, jobject obj, jbyteArray b, jint size, jint args, jstring jStr) {
	int head, tail, newVar, newVar2;
	int i = 1;
	int rc;
	
	//long T0 = clock(), T1, T2, T3, T4;
	jbyte *bytes; char *argString;
	
	//printf("C:Entering put_bytes - %d mS",T0);
	
	bytes = (*env)->GetByteArrayElements(env, b, 0);
	
	//T1 = clock() - T0;
	//printf("C:got bytes array - %d mS",T1);
	
	argString = (*env)->GetStringUTFChars(env, jStr, 0);
	c2p_functor(argString, args, reg_term(1));
	c2p_list(p2p_arg(reg_term(1),1));
	if (args == 3) {
		newVar = p2p_arg(reg_term(1),2);
		newVar = p2p_new();
		newVar2 = p2p_arg(reg_term(1),3);
		newVar2 = p2p_new();
	}
	head = p2p_car(p2p_arg(reg_term(1),1));
	tail = p2p_cdr(p2p_arg(reg_term(1),1));
	if (bytes[0]<0)
		c2p_int((bytes[0]+256), head);
	else
		c2p_int(bytes[0], head);
	while (i < size) {
		c2p_list(tail);
		head = p2p_car(tail);
		tail = p2p_cdr(tail);
		if (bytes[i]<0)
			c2p_int((bytes[i]+256), head);
		else
			c2p_int(bytes[i], head);
		i++;
	}
	c2p_nil(tail);
	
	//T2 = clock() - T0 ;
	//printf("C:constructed Prolog list - %d mS",T2);

	theEnv = env;
	theObj = obj;
	rc = xsb_query();
	
	//T3 = clock() - T0;
	//printf("C:Returned from Prolog - %d mS",T3);
	
	(*env)->ReleaseByteArrayElements(env, b, bytes, 0);
	(*env)->ReleaseStringUTFChars(env,jStr, argString);
	
	//T4 = clock() - T0;
	//printf("C:leaving put_bytes - %d mS\n",T4);
	return rc;
}


JNIEXPORT jint JNICALL 
Java_com_xsb_interprolog_NativeEngine_xsb_1close_1query
(JNIEnv *env , jobject obj) {
	return xsb_close_query();
}


JNIEXPORT jint JNICALL 
Java_com_xsb_interprolog_NativeEngine_xsb_1command_1string
(JNIEnv *env, jobject obj, jstring jCommandString) {
	int rcode;
	char *CommandString = (*env)->GetStringUTFChars(env, jCommandString, 0);
	theEnv = env;
	theObj = obj;
	rcode=xsb_command_string(CommandString);
	(*env)->ReleaseStringUTFChars(env,jCommandString, CommandString);
	return rcode;
}

JNIEXPORT jint JNICALL 
Java_com_xsb_interprolog_NativeEngine_xsb_1init_1internal
(JNIEnv *env, jobject obj, jstring jXSBPath) {
	int rcode;
	int myargc=2;
	char * myargv[2];
	char *XSBPath = (*env)->GetStringUTFChars(env, jXSBPath, 0);
	if (debug==JNI_TRUE) printf("Entering Java_com_xsb_interprolog_NativeEngine_xsb_1init_1internal\n");
	myargv[0] = XSBPath;
	myargv[1]="-n";
	
	rcode=xsb_init(myargc,myargv);
	(*env)->ReleaseStringUTFChars(env,jXSBPath, XSBPath);
	if (debug==JNI_TRUE) printf("Exiting Java_com_xsb_interprolog_NativeEngine_xsb_1init_1internal\n");
	return rcode;
}

JNIEXPORT void JNICALL 
Java_com_xsb_interprolog_NativeEngine_xsb_1interrupt (JNIEnv *env, jobject obj){
	/* Do XSB's "interrupt" thing, by simulating a ctrl-C: */
	// cf. keyint_proc(SIGINT) in subp.c; 
	// *asynint_ptr |= KEYINT_MARK; 
	keyint_proc(SIGINT);
}

JNIEXPORT void JNICALL Java_com_xsb_interprolog_NativeEngine_xsb_1setDebug
  (JNIEnv * env, jobject obj, jboolean d){
  	debug = d;
}

  
// the XSB Prolog built-in used in interprolog.P
// arguments: +length, +byte list, -new byte list
xsbBool interprolog_callback() {
	JNIEnv *env = theEnv;
	jobject obj = theObj;
	jclass cls;
	jmethodID mid;
	int i = 1, size, newHead, newTail;
	jbyte *b;
	jbyteArray newBytes, bytes;
	
	cls = (*env)->GetObjectClass(env, obj);
	if (cls == NULL) {
		printf("Could not find the class!\n");
		return 0;
	}
	//printf("Got the class\n");
	mid = (*env)->GetMethodID(env, cls, "callback", "([B)[B");
	if (mid == NULL) {
		printf("Could not find the method!\n");
		return 0;
	}
	//printf("Got the method\n");
	
	size = p2c_int(reg_term(1));
	
	b = (jbyte *) malloc(size);
	newHead = p2p_car(reg_term(2));
	newTail = p2p_cdr(reg_term(2));
	b[i-1] = p2c_int(newHead);
	while(is_nil(newTail) == 0) { 
		newHead = p2p_car(newTail);
		b[i] = p2c_int(newHead);
		newTail = p2p_cdr(newTail);
		i++;
	}
	bytes = (*env)->NewByteArray(env, size);
	(*env)->SetByteArrayRegion(env, bytes, 0, size, b);
	
	// Calls the method with bytes, expecting the return in newBytes
	newBytes = (*env)->CallObjectMethod(env, obj, mid, bytes);
	size = (*env)->GetArrayLength(env, newBytes);
	b = (*env)->GetByteArrayElements(env, newBytes, 0);
	c2p_list(reg_term(3));
	newHead = p2p_car(reg_term(3));
	newTail = p2p_cdr(reg_term(3));
	if (b[0]<0)
		c2p_int((b[0]+256), newHead);
	else
		c2p_int(b[0], newHead);
	i = 1;
	while (i < size) {
		c2p_list(newTail);
		newHead = p2p_car(newTail);
		newTail = p2p_cdr(newTail);
		if (b[i]<0)
			c2p_int((b[i]+256), newHead);
		else
			c2p_int(b[i], newHead);
		i++;
	}
	c2p_nil(newTail);
	return 1;
}
