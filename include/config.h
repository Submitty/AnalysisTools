#ifndef CONFIG_H
#define CONFIG_H

/*
 * 128^2 seems to be a happy medium for hash size despite not
 * matching up cleanly to any specific byte-length of keys.
 * Allowing keys to take the full two-byte range triples runtime
 * with almost the same results, while 256 * 16 (conveniently
 * representable as three hexadecimal digits) has terrible results
 * alongside very poor performance due to the dynamic allocation
 * necessary on a collision.
 */
#define HASH_BOUND (128 * 128)

#define FINGERPRINT_CACHE_SIZE 256

#define WORKING_DIR "moss_data"
#define GLOBAL_FILE_NAME "__global_fingerprint_count__"

#define SHARED_THRESHOLD 0.6

#define UPPER_BOUND 30
#define LOWER_BOUND 15

#endif
