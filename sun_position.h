#ifndef SUN_POSITION_H
#define SUN_POSITION_H

#ifdef _WIN32
#define DLL_IMPORT __declspec(dllimport)
#else
#define DLL_IMPORT
#endif

// Match the Fortran type
typedef struct {
    double Az;
    double El;
} AzElTiesol;

// Match TimeDateTiesol from Fortran
typedef struct {
    int year;
    int month;
    int day;
    int hour;
    int minute;
    double second;
} TimeDateTiesol;

// Declaration of the Fortran function
DLL_IMPORT AzElTiesol SUN_POSITION(
    TimeDateTiesol DateAndTime,
    double Latitude,
    double Longitude,
    double Elevation,
    double PressMilliBar,
    double TempC
);

#endif
