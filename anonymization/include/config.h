#ifndef CONFIG_H
#define CONFIG_H

/*
 * Length used for character buffers holding paths.
 */
#ifndef STRING_LENGTH
#define STRING_LENGTH 1024
#endif

/*
 * The directory in which the system saves metadata for each execution.
 */
#ifndef WORKING_DIR
#define WORKING_DIR ".analysis_data"
#endif

/*
 * Pattern used to redact matched names with no provided replacement in
 * the anonymization tool.
 */
#ifndef REDACTION_PATTERN
#define REDACTION_PATTERN "REDACTED%04u"
#endif

#ifndef ANONIMIZATION_HASH_BOUND
#define ANONIMIZATION_HASH_BOUND 9999
#endif

#endif
