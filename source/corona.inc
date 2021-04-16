// Handle infection data provided by the Robert-Koch-Institut, Germany
{$DEFINE RKI}

// Use barseries plots -- not supported any more because bars disappear when there
// are too many data points. Define left as a reminder that it once existed...
{.$DEFINE USE_BARSERIES}

// Compiles the program such that it is portable, i.e. all data and config files
// are at the same place
// Note: Do not set this option here, but in the project options where it is
// activated for RELEASE_PORTABLE buildmode only. Mentioned here for completeness.
{.$DEFINE Portable}

