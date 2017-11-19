PROJECT = validator
PROJECT_DESCRIPTION = Data Validator
PROJECT_VERSION = 1.0.0

ERLC_OPTS = +warn_missing_spec
ERLC_OPTS += +warn_unused_vars +warn_unused_function
ERLC_OPTS += +warn_unused_import +warn_unused_record
ERLC_OPTS += +warn_deprecated_function +warn_deprecated_type
ERLC_OPTS += +warn_export_vars +warn_shadow_vars +warn_obsolete_guard
ERLC_OPTS += +warn_export_all +warn_untyped_record
ERLC_OPTS += +debug_info +bin_opt_info
#ERLC_OPTS += +warnings_as_errors

include erlang.mk
