/*  (C) Copyright 1990 - 2014 by Wade L. Hennessey. All rights reserved. */

#include "lisp.h"
#include <stdio.h>
#include <sys/time.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

FILE *get_file_ptr(int index) {
  switch (index) {
  case 0: return(stdin);
  case 1: return(stdout);
  case 2: return(stderr);
  default: 
    printf("get_file_ptr: %d too large\n", index);
    return(0);
  }
}

/* Stuff like this should go away when we have a better foreign interface. */
int file_write_date(char *filename) {
  struct stat buffer;    
  int status;
  
  status = stat(filename,&buffer);
  if (status == 0) {
    return(buffer.st_mtime);
  } else {
    return(-1);
  }
}

int file_length(FILE *f) {
  struct stat buffer;    
  int status;
  
 status = fstat(f->_fileno,&buffer);

  if (status == 0) {
    return(buffer.st_size);
  } else {
    return(-1);
  }
}


LP truename(char *path, int result_type) {
  char *result;
  char truename[MAXPATHLEN];
  
  result = realpath(path,truename);
  if (result == NULL) {
    return(0);
  } else {
    switch (result_type) {
    case 0: return((LP) strdup(truename));	/*  result for C */
    case 1: return(copy_c_to_lisp_string(truename)); /*  result for Lisp */
    default: return(0);
    }
  }
}

LP probe_file(char *filename) {
  return((LP) truename(filename,1));
}

int fd_listen(int fd) {
  fd_set readfds;
  struct timeval timeout;
  int width,status;

  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
  width = fd + 1;
  FD_ZERO(&readfds);
  FD_SET(fd,&readfds);
  status = select(width,&readfds,NULL,NULL,&timeout);
/*  printf("Status: %d, isset: %d\n",status,FD_ISSET(fd,&readfds)); */
  return(FD_ISSET(fd,&readfds));
}

int file_listen(FILE *file) {
  return(fd_listen(file->_fileno));
}

LP read_byte(FILE *file, int element_type, LP eof_error_p, LP eof_value) {
  char buffer[4];
  int count,result;

  switch (element_type) {
  case ELEMENT_TYPE_BIT:
    printf("read-byte isn't suppported on stream of element-type BIT");;
    break;
  case ELEMENT_TYPE_SIGNED_8BIT:
    count = fread(buffer,1,1,file);
    result = (*((char *) buffer));
    break;
  case ELEMENT_TYPE_UNSIGNED_8BIT:
    count = fread(buffer,1,1,file);
    result = (*((unsigned char *) buffer));
    break;
  case ELEMENT_TYPE_SIGNED_16BIT:
    count = fread(buffer,2,1,file);
    /* Implicit signed - k+r won't eat explicit signed */
    result = (*((short *) buffer));
    break;
  case ELEMENT_TYPE_UNSIGNED_16BIT:
    count = fread(buffer,2,1,file);
    result = (*((unsigned short *) buffer));
    break;
  case ELEMENT_TYPE_SIGNED_32BIT:
    count = fread(buffer,4,1,file);
    /* HEY! make a bignum if needed */
    result = (*((unsigned long *) buffer));
    break;
  case ELEMENT_TYPE_UNSIGNED_32BIT:
    count = fread(buffer,4,1,file);
    /* HEY! make a bignum if needed */
    /* Implicit signed - k+r won't eat explicit signed */
    result = (*((long *) buffer));
    break;
  default:
    printf("Bogus stream element type\n");
    return(0);
  }
  if (count == 0) {
    if (eof_error_p == NIL) {
      return(eof_value);
    } else {
      printf("Read end-of-file while in read-byte\n");
      lisp_debug();
    }
  } else {
    return((LP) LONG_TO_FX(result));
  }
}

int write_byte(int element, FILE *file, int element_type) {
  char buffer[4];
  int status;

  switch (element_type) {
  case ELEMENT_TYPE_BIT:
    printf("write-byte isn't suppported on stream of element-type BIT");;
    break;
  case ELEMENT_TYPE_SIGNED_8BIT:
    /* Implicit signed - k+r won't eat explicit signed */
    *((char *) buffer) = element;
    status = fwrite(buffer,1,1,file);
    break;
  case ELEMENT_TYPE_UNSIGNED_8BIT:
    *((unsigned char *) buffer) = element;
    status = fwrite(buffer,1,1,file);
    break;
  case ELEMENT_TYPE_SIGNED_16BIT:
    /* Implicit signed - k+r won't eat explicit signed */
    *((short *) buffer) = element;
    status = fwrite(buffer,2,1,file);
    break;
  case ELEMENT_TYPE_UNSIGNED_16BIT:
    *((unsigned short *) buffer) = element;
    status = fwrite(buffer,2,1,file);
    break;
  case ELEMENT_TYPE_SIGNED_32BIT:
    /* Implicit signed - k+r won't eat explicit signed */
    *((long *) buffer) = element;
    status = fwrite(buffer,4,1,file);
    break;
  case ELEMENT_TYPE_UNSIGNED_32BIT:
    *((unsigned long *) buffer) = element;
    status = fwrite(buffer,4,1,file);
    break;
  default:
    printf("Bogus stream element type\n");
  }
  return(status);
}

LP read_vector(FILE *file,
	       int element_size,
	       LP vector,
	       int start,
	       int end,
	       LP eof_error_p,
	       LP eof_value) {
  int count,n;

  n = end - start;
  element_size = element_size >> 3; /* convert bits to bytes */
  count = fread(REMOVE_TAG(vector) + (start * element_size),
		element_size,n,file);
  if (count == 0) {
    if (eof_error_p == NIL) {
      return(eof_value);
    } else {
      printf("Read end-of-file while in read-vector\n");
      lisp_debug();
    }
  } else {
    return((LP) LONG_TO_FX(count));
  }
}

int write_vector(FILE *file, int element_size, LP vector, int start, int end) {
  int status,n;

  n = end - start;
  element_size = element_size >> 3; /* convert bits to bytes */
  status = fwrite(REMOVE_TAG(vector) + (start * element_size),
		  element_size,n,file);
  return(status);
}

int string_column(char *string, int current_column) {
  while (*string != 0) {
    if (*string == '\n') {
      current_column = 0;
    } else {
      current_column = current_column + 1;
    }
    string = string + 1;
  }
  return(current_column);
}
