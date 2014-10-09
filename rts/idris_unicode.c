#include <idris_rts.h>

/*
Implementation of primitive String operations in C
*/

int utf8_encode_char(int uchar, int* size, char (*out)[4]);
int utf8_decode_char(char bytes[4], int* uchar);

VAL allocateStringStorage(VM* vm, size_t byte_count, size_t char_count);
VAL allocateString(VM* vm, VAL storage, size_t offset, size_t char_count);


/* fill in a Closure with the empty string */
void idris_setEmptyString(VAL out){
  SETTY(out, USTRING);
  out->info.ustr = __idris_emptyUStr;
}

/* allocate a new empty string VAL */
VAL idris_allocateEmptyString(VM* vm){
  VAL cl = allocate(vm, sizeof(Closure), 0);
  idris_setEmptyString(cl);
  return cl;
}

/* allocate storage for raw string data */
VAL allocateStringStorage(VM* vm, size_t byte_count, size_t char_count){
    VAL cl;
    unsigned char* store;
    UStringStorage* storage;

    /* allocating three things here, we don't want 2 or 3 to cause 1 or 2 to move */
    idris_requireAlloc(vm, byte_count + sizeof(Closure) + sizeof(UStringStorage));
      store = allocate(vm, byte_count, 0);
      storage = allocate(vm, sizeof(UStringStorage), 0);
      cl = allocate(vm, sizeof(Closure), 0);
    idris_doneAlloc(vm);

    storage->store = store;
    storage->byte_count = byte_count;
    starage->char_count = char_count;

    SETTY(cl, USTORAGE);
    cl->info.ustorage = storage;
    return cl;
}

/* allocate a new string VAL */
VAL allocateString(VM* vm, VAL orig_storage, size_t offset, size_t char_count){
  VAL cl;
  UString* str;
  VAL storage = orig_storage;

  /* allocating two things here, we don't want the second one to cause the first to move */
  idris_requireAlloc(vm, sizeof(Closure) + sizeof(UString));
    cl = allocate(vm, sizeof(Closure), 0);
    str = allocate(vm, sizeof(UString), 0);
  idris_doneAlloc(vm);
  
  /* and str needs to point to storage, which may have moved in response to allocation */
  if(orig_storage->ty == FWD) storage = orig_storage->info.ptr;

  str->storage = storage;
  str->offset = offset;
  str->char_count = char_count;

  SETTY(cl, USTRING);
  cl->info.ustr = str;
  
  return cl;
}




/* utf8 encode a list of character codepoints into preallocated buffer */
/* returns -1 on error, 0 otherwise */
int idris_stringPack(VAL uchars, unsigned char out[]){
  unsigned char* optr = out;
  unsigned char utf8[4] = {0,0,0,0};
  VAL vptr = uchars;
  int size = 0;

  do {
    if(CARITY(vptr) == 0) return 0; // end of list reached

    if(utf8_encode_char(GETARG(vptr,0)->info.i, &size, utf8) < 0) return -1; // bad codepoint

    if(size == 1){
      optr[0] = utf8[0];
      optr += 1;
    }
    else if(size == 2){
      optr[0] = utf8[0];
      optr[1] = utf8[1];
      optr += 2;
    }
    else if(size == 3){
      optr[0] = utf8[0];
      optr[1] = utf8[1];
      optr[2] = utf8[2];
      optr += 3;
    }
    else if(size == 4){
      optr[0] = utf8[0];
      optr[1] = utf8[1];
      optr[2] = utf8[2];
      optr[3] = utf8[3];
      optr += 4;
    }
    else{
      fprintf(stderr, "idris_stringPack impossible case encountered, size = %d\n", size);
      exit(-1);
    }

    vptr = GETARG(vptr,1);

    #ifdef DEBUG
    /* check that vptr points to a list node or empty list */
    #endif

  } while(1); // terminates only if passed a valid (acyclic) linked list
}



/* decompose the string into a char and allocate a new view of the tail */
/* crash if the strings storage contains invalid utf8 */
/* return -1 if string is empty */
int idris_stringUncons(VM* vm, VAL str, int* uchar, VAL* tail){
  int head;
  int size;
  int err;
  VAL cl;

  if(idris_stringLength(str) == 0) return -1;

  err = utf8_decode_char(str->info.ustr->str->info.str, &head, &size);
  if(err < 0){
    fprintf(stderr, "idris_stringUncons encountered invalid utf8\n");
    exit(-1);
  }

  if(idris_stringLength(str) == 1){
    cl = idris_allocateEmptyString(vm);
  }
  else{
    cl = allocateString(
      VM* vm,
      str->info.ustr->storage,
      str->info.ustr->offset + size,
      str->info.ustr->char_count - 1
    );
  }

  *uchar = head;
  *tail = cl;
  return 0;
}

/* join a list of strings separated by string delim into preallocated buffer */
void idris_stringJoin(VAL delim, VAL strs, VAL out){
}

/* reverse a string into a preallocated buffer */
void idris_stringReverse(VAL str, VAL out){
}

/* if string needle is not found in string haystiack return -1 */
/* else allocate two views into the haystack split at first occurrence and return 0 */
/*   one is the prefix ending just before the needle */
/*   two is the rest of the string starting with needle */
int idris_stringBreakOn(VAL needle, VAL haystack, VAL* out1, VAL* out2){
  return -1;
}

/* if i >= length of string return -1 */
/* else allocate two views into str split at index i */
int idris_stringSplitAt(int i, VAL str, VAL* out1, VAL* out2){
  return -1;
}

/* return number char count of the string */
int idris_stringLength(VAL str){
  return str->info.ustr->char_count;
}





/* write a utf8 byte sequence for character uchar and return the size in bytes in size. */
/* return -1 in case uchar is invalid, otherwise return 0 */
int utf8_encode_char(int uchar, int* size, unsigned char (*out)[4]){
  int c = uchar;

  if(
    (c < 0 || c > 0x10ffff) || // out of unicode range
    (c == 0xfffe || c == 0xffff) || // half of a BOM
    (c >= 0xd800 && c <= 0xdfff) // utf16 half of a surrogate pair
  ){
    fprintf(stderr, "utf8_encode_char bad codepoint encountered c = %x\n", c);
    return -1;
  }

  if(c <= 0x7f){
    out[0] = c;
    *size = 1;
  }
  else if(c <= 0x7ff){
    out[0] = 0xc0 | (c >> 6);
    out[1] = 0x80 | (c & 0x3f);
    *size = 2;
  }
  else if(c <= 0x7fff){
    out[0] = 0xe0 | (c >> 12);
    out[1] = 0x80 | ((c >> 6) & 0x3f);
    out[2] = 0x80 | (c        & 0x3f);
    *size = 3;
  }
  else{ // c <= 0x10ffff see above
    out[0] = 0xf0 | (c >> 18);
    out[1] = 0x80 | ((c >> 12) & 0x3f);
    out[2] = 0x80 | ((c >> 6)  & 0x3f);
    out[3] = 0x80 | (c         & 0x3f);
    *size = 4;
  }

  return 0;
}

/* read a utf8 byte sequence to get a character codepoint. */
/* return -1 if there is an error, otherwise return 0 */
int utf8_decode_char(char bytes[4], int* uchar, int* size){
  return -1;
}
