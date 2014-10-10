#include <idris_rts.h>

/*
Implementation of primitive String operations in C
*/

/* need to implement following
VAL idris_stringCons(VM* vm, VAL c, VAL str);            CHECK
VAL idris_stringAppend(VM* vm, VAL str1, VAL str2);
VAL idris_stringUnsafeHead(VM* vm, VAL str);             CHECK
VAL idris_stringUnsafeTail(VM* vm, VAL str);
VAL idris_stringLength(VM* vm, VAL str);                 CHECK
VAL idris_stringReverse(VM* vm, VAL str);
VAL idris_stringSlice(VM* vm, VAL i0, VAL i1, VAL str);
VAL idris_stringCompare(VM* vm, VAL str1, VAL str2);

*/

int utf8_encode_char(int uchar, int* size, unsigned char (*out)[4]);
int utf8_decode_char(char bytes[4], int* uchar, int* size);

VAL idris_stringCons(VM* vm, VAL c, VAL str){
  unsigned char utf8[4];
  int encoded_char_size;

  int ret = utf8_encode_char(c->info.i, &encoded_char_size, utf8);
  if(ret < 0) exit(-1); /* crash on bad codepoint */

  int store_size;
  int offset;
  int total_allocation;
  String string;
  Storage storage;
  VAR string_cl;
  VAR storage_cl;

  if(str == NULL){ /* empty string, so allocate new singleton string */
    /* allocate storage of at least 8 bytes to put a single char of up to 4 bytes */
    /* put it somewhere in the middle of the region */
    store_size = next_power_of_two(encoded_char_size);
    store_size = store_size < 8 ? 8 : store_size;
    offset = store_size / 2;
    total_allocation = 
      sizeof(Closure) + sizeof(String) +
      sizeof(Closure) + sizeof(Storage) + store_size;

    idris_requireAlloc(vm, total_allocation);

      new_store = allocate(vm, store_size*sizeof(unsigned char), 0);
      memcpy(utf8, new_store+offset, encoded_char_size);

      storage = allocate(vm, sizeof(Storage), 0);
      storage->store = new_store;
      storage->size = store_size;
      storage->lower_bound = offset;
      storage->upper_bound = offset + encoded_char_size - 1;

      storage_cl = allocate(vm, sizeof(Closure), 0);
      SETTY(cl, STORAGE);
      storage_cl->info.storage = storage;

      string = allocate(vm, sizeof(String), 0);
      string->storage = storage_cl;
      string->offset = offset;
      string->byte_count = encoded_char_size;
      string->char_count = 1;

      string_cl = allocate(vm, sizeof(Closure), 0);
      SETTY(cl, STRING);
      string_cl->info.string = string;

    idris_doneAlloc(vm);

    return string_cl;
  }
  else if(str->info.string->offset - encoded_char_size < str->info.string->storage->info.storage->lower_bound){ /* there is room in the storage for the character, use it */

    memcpy(utf8, str->info.string->storage->info.storage->store - encoded_char_size, encoded_char_size);
    str->info.string->storage->info.storage->lower_bound -= encoded_char_size;

    idris_requireAlloc(vm, sizeof(String) + sizeof(Closure));

      string = allocate(vm, sizeof(String), 0);
      string->storage = str->info.string->storage;
      string->offset = str->info.string->offset - encoded_char_size;
      string->byte_count = str->info.string->byte_count + encoded_char_size;
      string->char_count = str->info.string->char_count + 1;

      string_cl = allocate(vm, sizeof(Closure), 0);
      SETTY(cl, STRING);
      string_cl->info.string = string;

    idris_doneAlloc(vm);

    return string_cl;
  }
  else{ /* can't put char in existing storage without going out of bounds or clobbering something important, so copy into a new expanded region */

    string_size = str->info.string->byte_count + encoded_char_size;
    store_size = next_power_of_two(string_size);
    store_size = store_size < 8 ? 8 : store_size;
    offset = store_size - string_size / 2;
    total_allocation = 
      sizeof(Closure) + sizeof(String) +
      sizeof(Closure) + sizeof(Storage) + store_size;

    idris_requireAlloc(vm, total_allocation);

      new_store = allocate(vm, store_size*sizeof(unsigned char), 0);
      memcpy(utf8, new_store+offset, encoded_char_size);
      memcpy(
        str->info.string->storage->info.storage->store + str->info.string->offset,
        new_store + offset + encoded_char_size,
        str->info.string->byte_count
      );

      storage = allocate(vm, sizeof(Storage), 0);
      storage->store = new_store;
      storage->size = store_size;
      storage->lower_bound = offset;
      storage->upper_bound = offset + string_size - 1;

      storage_cl = allocate(vm, sizeof(Closure), 0);
      SETTY(cl, STORAGE);
      storage_cl->info.storage = storage;

      string = allocate(vm, sizeof(String), 0);
      string->storage = storage_cl;
      string->offset = offset;
      string->byte_count = string_size;
      string->char_count = str->info.string->char_count + 1;

      string_cl = allocate(vm, sizeof(Closure), 0);
      SETTY(cl, STRING);
      string_cl->info.string = string;

    idris_doneAlloc(vm);

    return string_cl;
  }

}

VAL idris_stringAppend(VM* vm, VAL str1, VAL str2){
  /*
try to copy str1 into the prefix of str2's storage
if not, try to copy str2 into str1's storage suffix
if not, allocate new storage to hold the result and memcpy str1 and str2 into it
  */

  return NULL;
}

VAL idris_stringUnsafeHead(VM* vm, VAL str){
  if(str == NULL){
    fprintf(stderr, "stringUnsafeHead: empty string\n");
    exit(-1);
  }
  else{
    int c;
    int size;

    int ret = utf8_decode_char(str->info.string->storage->info.storage->store + str->info.string->offset, &c, &size);

    if(ret < 0){
      fprintf(stderr, "stringUnsafeHead: invalid utf8\n");
      exit(-1);
    }

    return MKINT(c);
  }
}

VAL idris_stringUnsafeTail(VM* vm, VAL str){
  if(str == NULL){ /* unsafeTail on "" is an error */
    fprintf(stderr, "stringUnsafeTail: empty string\n");
    exit(-1);
  }
  else if(str->info.string->char_count == 1){ /* unsafeTail on singleton is "" */
    return NULL;
  }
  else{
    int c;
    int size;

    int ret = utf8_decode_char(str->info.string->storage->info.storage->store + str->info.string->offset, &c, &size);

    if(ret < 0){
      fprintf(stderr, "stringUnsafeTail: invalid utf8\n");
      exit(-1);
    }

    /* TODO */
    /* allocate a string slice into str's storage */
    return NULL;
  }
}

VAL idris_stringLength(VM* vm, VAL str){
  return MKINT(str ? str->info.string->char_count : 0);
}

VAL idris_stringReverse(VM* vm, VAL str){
  if(str == NULL) return NULL;

  /* allocate space for a reversal,
     traverse the string and encode the characters backwards in the new storage */
}

VAL idris_stringSlice(VM* vm, VAL i0, VAL i1, VAL str){
  if(str == NULL) return NULL;
  /* this should clamp if the indexes are out of range */
}

VAL idris_stringCompare(VM* vm, VAL str1, VAL str2){
  if(str1 == NULL && str2 == NULL) return MKINT(0);
  if(str1 == NULL) return MKINT(-1);
  if(str2 == NULL) return MKINT(1);
  /* compare byte-by-byte */
}


/********** dont look down */

/* compute a utf8 byte sequence for character uchar and return the size in bytes in size. */
/* return -1 in case uchar is invalid, otherwise return 0 */
int utf8_encode_char(int uchar, int* size, unsigned char (*out)[4]){
  int c = uchar;

  if(
    (c < 0 || c > 0x10ffff) || // out of unicode range
    (c == 0xfffe || c == 0xffff) || // half of a BOM
    (c >= 0xd800 && c <= 0xdfff) // half of a utf16 surrogate pair
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
