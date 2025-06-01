#include <stdio.h>
#include <windows.h>
#include "sun_position.h"

int main() {
    double Latitude     = 34.0;
    double Longitude    = -118.0;
    double Altitude     = 0.3;
    double Pressure     = 1013.25;
    double Temperature  = 20.0;
    
    TimeDateTiesol DateAndTime;
    DateAndTime.year   = 2025;
    DateAndTime.month  = 5;
    DateAndTime.day    = 16;
    DateAndTime.hour   = 19;      /* Note: Tiesol time is in the UTC zone, presently 7 hours ahead of Tonopah (PDT) */
    DateAndTime.minute = 0;
    DateAndTime.second = 0.0;

    AzElTiesol angles = SUN_POSITION(DateAndTime, Latitude, Longitude, Altitude, Pressure, Temperature);
   
    for (int second = 0; second < 60; second++) {
      DateAndTime.second = second;
      angles = SUN_POSITION(DateAndTime, Latitude, Longitude, Altitude, Pressure, Temperature);
      
      printf("Azimuth  : %f\n", angles.Az);
      printf("Elevation: %f\n", angles.El);
      Sleep(1000);
    }

    return 0;
}
