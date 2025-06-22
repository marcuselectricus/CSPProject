/* Heliostat Field */
CREATE TABLE Field(
  Instance_Field INTEGER PRIMARY KEY,                                  /* Primary Key for the Field Table */
  WflowClearSky_Field DOUBLE PRECISION DEFAULT 0.0,                    /* The clear sky flow of the field (lb/s) */
  PressureECVClearSky_Field DOUBLE PRECISION DEFAULT 0.0,              /* Data tracked at the field level: */
  NumHeliostatsTrack_Field INTEGER DEFAULT 0,                          /* Number of heliostats in the tracking mode */
  NumHeliostatsTracking_Field INTEGER DEFAULT 0,                       /* Number of heliostats tracking */
  NumHeliostatsStow_Field INTEGER DEFAULT 0,                           /* Number of heliostats in emergency stow mode */
  NumHeliostatsStowed_Field INTEGER DEFAULT 0,                         /* Number of heliostats at emergency stow */
  NumHeliostatsDefocus_Field INTEGER DEFAULT 0,                        /* Number of heliostats in emergency defocus mode */
  NumHeliostatsDefocused_Field INTEGER DEFAULT 0,                      /* Number of heliostats at emergency defocus */
  NumHeliostatsSleep_Field INTEGER DEFAULT 0,                          /* Number of heliostats in sleep mode */
  NumHeliostatsSleeping_Field INTEGER DEFAULT 0,                       /* Number of heliostats asleep */
  NumHeliostatsWash_Field INTEGER DEFAULT 0,                           /* Number of heliostats in rainwash mode */
  NumHeliostatsAtWash_Field INTEGER DEFAULT 0,                         /* Number of heliostats at wash position */
  NumHeliostatsAtRainWash_Field INTEGER DEFAULT 0,                     /* Number of heliostats at rain wash position */
  NumHeliostatsRainWash_Field INTEGER DEFAULT 0,                       /* Number of heliostats in wash mode */
  NumHeliostatsAtDefrost_Field INTEGER DEFAULT 0,                      /* Number of heliostats at defrost position */
  NumHeliostatsDefrost_Field INTEGER DEFAULT 0,                        /* Number of heliostats in defrost mode */
  NumHeliostatsManual_Field INTEGER DEFAULT 0,                         /* Number of heliostats in manual position mode */
  NumHeliostatsAtManual_Field INTEGER DEFAULT 0,                       /* Number of heliostats at the manually position */
  NumHeliostatsStop_Field INTEGER DEFAULT 0,                           /* Number of heliostats in emergency stop mode */
  NumHeliostatsStopped_Field INTEGER DEFAULT 0,                        /* Number of heliostats succefully stopped for emergency purposes */
  NumHeliostatsStandby_Field INTEGER DEFAULT 0,                        /* Number of heliostats in standby mode */
  NumHeliostatsAtStandby_Field INTEGER DEFAULT 0,                      /* Number of heliostats at the standby position */
  NumHeliostatsBCS_Field INTEGER DEFAULT 0,                            /* Number of heliostats being processed by the BCS */
  NumHeliostatsAtBCS_Field INTEGER DEFAULT 0,                          /* Number of heliostats being imaged by the BCS */
  NumHeliostatsAvailable_Field INTEGER DEFAULT 0,                      /* Number of heliostats able to be controlled */
  NumHeliostatsUnavailable_Field INTEGER DEFAULT 0,                    /* Number of heliostats unable to be controlled */
  NumHeliostatsTransition_Field INTEGER DEFAULT 0,                     /* Number of heliostats in transition */
  NumHeliostatsHFCSControl_Field INTEGER DEFAULT 0,                    /* Number of heliostats under HFCS control */
  NumHeliostatsGoodEncoders_Field INTEGER DEFAULT 0,                   /* Number of heliostats with encoder confidence */
  NumHeliostatsSeekingMark_Field INTEGER DEFAULT 0,                    /* Number of heliostats recalibrating encoder */
  NumHeliostatsOffLine_Field INTEGER DEFAULT 0,                        /* Number of heliostats in the off-line state */
  NumHeliostatsError_Field INTEGER DEFAULT 0,                          /* Number of heliostats in an error state */
  NumHFCUnknownState_Field INTEGER DEFAULT 0,                          /* Number of HFCs in an unknown state */
  NumHFCGoodState_Field INTEGER DEFAULT 0,                             /* Number of HFCs in an good state */
  NumHFCBadState_Field INTEGER DEFAULT 0,                              /* Number of HFCs in an bad state */
  NumHeliostatsAnyWash_Field INTEGER DEFAULT 0,                        /* Number of heliostats in either rain wash or ordinary wash mode */
  NumHeliostatsAtAnyWash_Field INTEGER DEFAULT 0,                      /* Number of heliostats at either rain wash or ordinary wash position */
  NumHeliostatsAnyStow_Field INTEGER DEFAULT 0,                        /* Number of heliostats in either stow, sleep or defocus mode */
  NumHeliostatsAtAnyStow_Field INTEGER DEFAULT 0,                      /* Number of heliostats at either stow, sleep or defocus position */
  PrepareForBCS$Heliostat_Field INTEGER DEFAULT 0,                     /* The heliostat being evaluated for BCS scheduling */
  TimeTagBCSList_Field TIMESTAMPTZ DEFAULT now(),                      /* The time when the BCS list of heliostats was finalized */
  TimeTagBCSNext_Field TIMESTAMPTZ DEFAULT now(),                      /* The next time when the BCS list of heliostats will be started */
  ID_Field VARCHAR(32) DEFAULT '',                                     /* The name of a field */
  I$Place_Field INTEGER DEFAULT 0,                                     /* The location of a field */
  I$Htype_Field INTEGER DEFAULT 0,                                     /* The type of heliostat used to layout the field */
  AreaGlass_Field DOUBLE PRECISION DEFAULT 0.0,                        /* The total glass area for the field (m^2) */
  dSoilingPerDay_Field DOUBLE PRECISION DEFAULT 0.0,                   /* The measured soiling rate for the heliostats in the field (ratio/day) */
  AngleSunAzimuthal_Field DOUBLE PRECISION DEFAULT 0.0,                /* The sun azimuthal angle position (degrees) */
  AngleSunElevation_Field DOUBLE PRECISION DEFAULT 0.0,                /* The sun elevation angle position (degrees) */
  XPixelPositionSun_Field INTEGER DEFAULT 0,                           /* The sun horixontal position on the spider display (pixels) */
  YPixelPositionSun_Field INTEGER DEFAULT 0,                           /* The sun vertical position on the spider display (pixels) */
  XPixelPositionSunOneLine_Field INTEGER DEFAULT 0,                    /* The sun horixontal position on the one-line display (pixels) */
  YPixelPositionSunOneLine_Field INTEGER DEFAULT 0,                    /* The sun vertical position on the one-line display (pixels) */
  AngleMoonAzimuthal_Field DOUBLE PRECISION DEFAULT 0.0,               /* The moon azimuthal angle position (degrees) */
  AngleMoonElevation_Field DOUBLE PRECISION DEFAULT 0.0,               /* The moon elevation angle position (degrees) */
  XPixelPositionMoon_Field INTEGER DEFAULT 0,                          /* The moon horixontal position on the one-line display (pixels) */
  YPixelPositionMoon_Field INTEGER DEFAULT 0,                          /* The moon vertical position on the one-line display (pixels) */
  Type_Field INTEGER DEFAULT 0,                                        /* The type of focusing the field heliostats were manufactured to */
    FocusToSlantRange_Field BOOLEAN DEFAULT false,                     /* Individual focus with focal length equal to the slant range */
    FocusByZone_Field BOOLEAN DEFAULT false,                           /* Focus by zone with ocal length determined by XFocal_Zone and YFocal_Zone */
    FocusIndividually_Field BOOLEAN DEFAULT false,                     /* Individual heliostat focal length determined by XFocal_Heliostat YFocal_Heliostat */
  NumPageDocument_Field INTEGER DEFAULT 0,                             /* The number of pages in the document for this field */
  DateReleaseDocument_Field VARCHAR(16) DEFAULT '',                    /* The release time/date string for the document */
  NumPageLayout_Field INTEGER DEFAULT 0,                               /* The number of pages in the layout for this field */
  TimeDateStringLayout_Field VARCHAR(32) DEFAULT '',                   /* The release time/date string for the field layout */
  StatusSet_Field INTEGER DEFAULT 0,                                   /* The User Interface selected field commanded state and mode */
    StowSet_Field BOOLEAN DEFAULT false,                               /* The operator has commanded the Wind Stow state for the field */
    StandBySet_Field BOOLEAN DEFAULT false,                            /* The operator has commanded the Standby state for the field */
    PreHeatSet_Field BOOLEAN DEFAULT false,                            /* The operator has commanded the Preheat state for the field */
    GenerationSet_Field BOOLEAN DEFAULT false,                         /* The operator has commanded the Generation state for the field */
    PostHeatSet_Field BOOLEAN DEFAULT false,                           /* The operator has commanded the Postheat state for the field */
    DefocusSet_Field BOOLEAN DEFAULT false,                            /* The operator has commanded the Emergency Defocus state for the field */
    SleepSet_Field BOOLEAN DEFAULT false,                              /* The operator has commanded the Night Stow state for the field */
    RainWashSet_Field BOOLEAN DEFAULT false,                           /* The operator has commanded the Rainwash state for the field */
    DefrostSet_Field BOOLEAN DEFAULT false,                            /* The operator has commanded the Defrost state for the field */
    OperatorControlSet_Field BOOLEAN DEFAULT false,                    /* The operator has commanded the Operator Control Mode for the field */
    AutomaticControlSet_Field BOOLEAN DEFAULT false,                   /* The operator has commanded the Automatic Control Mode for the field */
    AnalystControlSet_Field BOOLEAN DEFAULT false,                     /* The operator has commanded the Analyst Control Mode for the field */
  DirectionWind_Field INTEGER DEFAULT 0,                               /* The wind direction of the field (For Display purposes) */
    North_Field BOOLEAN DEFAULT false,                                 /* The wind is coming from the North */
    NorthEast_Field BOOLEAN DEFAULT false,                             /* The wind is coming from the Northeast */
    East_Field BOOLEAN DEFAULT false,                                  /* The wind is coming from the East */
    SouthEast_Field BOOLEAN DEFAULT false,                             /* The wind is coming from the Southeast */
    South_Field BOOLEAN DEFAULT false,                                 /* The wind is coming from the South */
    SouthWest_Field BOOLEAN DEFAULT false,                             /* The wind is coming from the Southeast */
    West_Field BOOLEAN DEFAULT false,                                  /* The wind is coming from the West */
    NorthWest_Field BOOLEAN DEFAULT false,                             /* The wind is coming from the Northwest */
  CTRL$HFC_Field INTEGER DEFAULT 0,                                    /* The controlling HFC for the field (this will point to the DCS) */
  I$Receiver_Field INTEGER DEFAULT 0,                                  /* The receiver for this field (For display purposes). */
  Flow1$Circuit_Field INTEGER DEFAULT 0,                               /* The flow for Circuit 1 (For display purposes) */
  Flow2$Circuit_Field INTEGER DEFAULT 0,                               /* The flow for Circuit 2 (For display purposes) */
  StatusIn_Field INTEGER DEFAULT 0,                                    /* The data from the DCS */
    StowIn_Field BOOLEAN DEFAULT false,                                /* The DCS commands the field to the Wind Stow State */
    StandbyIn_Field BOOLEAN DEFAULT false,                             /* The DCS commands the field to the Standby State */
    PreHeatIn_Field BOOLEAN DEFAULT false,                             /* The DCS commands the field to the Preheat State */
    TrackIn_Field BOOLEAN DEFAULT false,                               /* The DCS commands the field to the Generation State */
    PostHeatIn_Field BOOLEAN DEFAULT false,                            /* The DCS commands the field to the Postheat State */
    PlantPowerAvailable_Field BOOLEAN DEFAULT false,                   /* Plant power is available */
    LoadCenter1Power_Field BOOLEAN DEFAULT false,                      /* Transformer Power for Load Center 1 is available */
    LoadCenter2Power_Field BOOLEAN DEFAULT false,                      /* Transformer Power for Load Center 2 is available */
    LoadCenter3Power_Field BOOLEAN DEFAULT false,                      /* Transformer Power for Load Center 3 is available */
    LoadCenter4Power_Field BOOLEAN DEFAULT false,                      /* Transformer Power for Load Center 4 is available */
    LoadCenter5Power_Field BOOLEAN DEFAULT false,                      /* Transformer Power for Load Center 5 is available */
    LoadCenter6Power_Field BOOLEAN DEFAULT false,                      /* Transformer Power for Load Center 6 is available */
    TripCKT1BackwallTemp_Field BOOLEAN DEFAULT false,                  /* Trip field for Circuit 1 Backwall overtemperature */
    TripCKT2BackwallTemp_Field BOOLEAN DEFAULT false,                  /* Trip field for Circuit 2 Backwall overtemperature */
    TripPushButtonHeliostats_Field BOOLEAN DEFAULT false,              /* DCS operator has commanded the field to trip */
  StatusQuality1In_Field INTEGER DEFAULT 0,                            /* Quality status word 1 from the DCS */
    Radiometer1Quality_Field BOOLEAN DEFAULT false,                    /* Radiometer 1 has good status */
    Radiometer2Quality_Field BOOLEAN DEFAULT false,                    /* Radiometer 2 has good status */
    Radiometer3Quality_Field BOOLEAN DEFAULT false,                    /* Radiometer 3 has good status */
    Radiometer4Quality_Field BOOLEAN DEFAULT false,                    /* Radiometer 4 has good status */
    Radiometer5Quality_Field BOOLEAN DEFAULT false,                    /* Radiometer 5 has good status */
    Radiometer6Quality_Field BOOLEAN DEFAULT false,                    /* Radiometer 6 has good status */
    PerformanceTestModeIn_Field BOOLEAN DEFAULT false,                 /* Performance test with the entire field. */
    Group1TestModeIn_Field BOOLEAN DEFAULT false,                      /* Power test with Group 1 heliostats */
    Group2TestModeIn_Field BOOLEAN DEFAULT false,                      /* Power test with Group 2 heliostats */
    Group1And2TestModeIn_Field BOOLEAN DEFAULT false,                  /* Power test with all heliostats */
    PowerTest25Pct_Field BOOLEAN DEFAULT false,                        /* Power test with 25 percent of the heliostats */
  StatusQuality2In_Field INTEGER DEFAULT 0,                            /* Quality status word 2 from the DCS */
    FlowInCircuit1Quality_Field BOOLEAN DEFAULT false,                 /* Flow Telemetry for Circuit 1 inlet valve is good */
    FlowInCircuit2Quality_Field BOOLEAN DEFAULT false,                 /* Flow Telemetry for Circuit 1 inlet valve is good */
    ShortTermFill_Field BOOLEAN DEFAULT false,                         /* The Receiver has been commanded to Short Term Fill */
    ShortTermCirculate_Field BOOLEAN DEFAULT false,                    /* The Receiver has been commanded to Short Term Circulate */
    ReceiverFill_Field BOOLEAN DEFAULT false,                          /* The Receiver has been commanded to Fill */
    ReceiverOperate_Field BOOLEAN DEFAULT false,                       /* The Receiver has been commanded to Operate */
    DCSHeartbeatPulseIn_Field BOOLEAN DEFAULT false,                   /* Heartbeat Pulse from the DCS */
    ReceiverDrain_Field BOOLEAN DEFAULT false,                         /* The Receiver has been commanded to Drain */
    ReceiverLongTermHold_Field BOOLEAN DEFAULT false,                  /* The Receiver has been commanded to Long Term Hold */
  StatusOut_Field INTEGER DEFAULT 0,                                   /* The Field status transmitted to the DCS */
    AtStandbyOut_Field BOOLEAN DEFAULT false,                          /* The Field is at Standby */
    AtPreHeatOut_Field BOOLEAN DEFAULT false,                          /* The Receiver is Preheated */
    AtPostHeatOut_Field BOOLEAN DEFAULT false,                         /* The Receiver has satisfied Postheat criteria */
    AlarmHFCSOut_Field BOOLEAN DEFAULT false,                          /* There is an alarm in the HFCS cabinets */
    AlarmHFCOut_Field BOOLEAN DEFAULT false,                           /* There is an alarm in the Field */
    HFCSHeartbeatPulseOut_Field BOOLEAN DEFAULT false,                 /* HFCS Heartbeat Pulse */
    AtEmergencyOut_Field BOOLEAN DEFAULT false,                        /* The Field has been succsssfully defocused */
    StowedOut_Field BOOLEAN DEFAULT false,                             /* The Field has been stowed */
    TrackingOut_Field BOOLEAN DEFAULT false,                           /* The Field is successfully tracking */
    DCSControlOut_Field BOOLEAN DEFAULT false,                         /* Flow Measurements */
  FluxDNI_Field DOUBLE PRECISION DEFAULT 0.0,                          /* The flux used for the controls (kw/m^2) */
  FluxDNIClearSky_Field DOUBLE PRECISION DEFAULT 0.0,                  /* The clear sky value of flux    (kw/m^2) */
  Visibility_Field DOUBLE PRECISION DEFAULT 0.0,                       /* The field atmospheric visibility (km) */
  SpeedWind_Field DOUBLE PRECISION DEFAULT 0.0,                        /* Field wind speed (m/s) */
  AngleWind_Field DOUBLE PRECISION DEFAULT 0.0,                        /* Direction of field windspeed, measured clockwise from north (degrees) */
  Psia_Field DOUBLE PRECISION DEFAULT 0.0,                             /* Field barometric pressure (Psia) */
  RelativeHumidity_Field DOUBLE PRECISION DEFAULT 0.0,                 /* Field relative humidity (Percent) */
  TemperatureF_Field DOUBLE PRECISION DEFAULT 0.0,                     /* Ambient Temperature (degF) */
  TemperatureC_Field DOUBLE PRECISION DEFAULT 0.0,                     /* Ambient Temperature (degC) */
  Temperature_Field DOUBLE PRECISION DEFAULT 0.0,                      /* Ambient Temperature in System Units */
  SourceData_Field INTEGER DEFAULT 0,                                  /* The souce of data (Telemetered, Estimated or Manually Replaced) */
    FluxDNITelemetry_Field BOOLEAN DEFAULT false,                      /* The telemetered value is used */
    FluxDNIEstimated_Field BOOLEAN DEFAULT false,                      /* The estimated value is used */
    FluxDNIManual_Field BOOLEAN DEFAULT false,                         /* The manually replaced value is used */
    VisibilityTelemetry_Field BOOLEAN DEFAULT false,                   /* The telemetered value is used */
    VisibilityEstimated_Field BOOLEAN DEFAULT false,                   /* The estimated value is used */
    VisibilityManual_Field BOOLEAN DEFAULT false,                      /* The manually replaced value is used */
    SpeedWindTelemetry_Field BOOLEAN DEFAULT false,                    /* The telemetered value is used */
    SpeedWindEstimated_Field BOOLEAN DEFAULT false,                    /* The estimated value is used */
    SpeedWindManual_Field BOOLEAN DEFAULT false,                       /* The manually replaced value is used */
    AngleWindTelemetry_Field BOOLEAN DEFAULT false,                    /* The telemetered value is used */
    AngleWindEstimated_Field BOOLEAN DEFAULT false,                    /* The estimated value is used */
    AngleWindManual_Field BOOLEAN DEFAULT false,                       /* The manually replaced value is used */
    PsiaTelemetry_Field BOOLEAN DEFAULT false,                         /* The telemetered value is used */
    PsiaEstimated_Field BOOLEAN DEFAULT false,                         /* The estimated value is used */
    PsiaManual_Field BOOLEAN DEFAULT false,                            /* The manually replaced value is used */
    HumidityTelemetry_Field BOOLEAN DEFAULT false,                     /* The telemetered value is used */
    HumidityEstimated_Field BOOLEAN DEFAULT false,                     /* The estimated value is used */
    HumidityManual_Field BOOLEAN DEFAULT false,                        /* The manually replaced value is used */
    TemperatureTelemetry_Field BOOLEAN DEFAULT false,                  /* The telemetered value is used */
    TemperatureEstimated_Field BOOLEAN DEFAULT false,                  /* The estimated value is used */
    TemperatureManual_Field BOOLEAN DEFAULT false,                     /* The manually replaced value is used */
  CommandFluxModel_Field INTEGER DEFAULT 0,                            /* The commanded state of the field when the flux model was calculated */
  TimeStampFluxModelUTC_Field VARCHAR(24) DEFAULT '',                  /* The Time when the controls were calculated (seconds) */
  StatusSetHeliostats_Field INTEGER DEFAULT 0,                         /* The UI operator can command the Field Mode. */
    HeliostatsAutomatic_Field BOOLEAN DEFAULT false,                   /* The operator has commanded the field to Automatic Control Mode */
    HeliostatsManual_Field BOOLEAN DEFAULT false,                      /* The operator has commanded the field to Manual Control Mode */
  PctPower_Field DOUBLE PRECISION DEFAULT 0.0,                         /* The UI operator entered percent of heliostats used for generation (percent) */
  PctPowerPreheat_Field DOUBLE PRECISION DEFAULT 0.0,                  /* The UI operator entered percent of heliostats used for preheat (percent) */
  PctPowerReceiverThaw_Field DOUBLE PRECISION DEFAULT 0.0,             /* The operator entered percent of heliostats that the operator has selected (percent) */
  SecondsTrackingSmoothly_Field INTEGER DEFAULT 0,                     /* Counter to determine if in "steady state", according to number of heliostats "in Track". */
  Command_Field INTEGER DEFAULT 0,                                     /* The commanded state and mode of the field */
    Stow_Field BOOLEAN DEFAULT false,                                  /* The Field has been commanded to the Wind Stow State */
    StandBy_Field BOOLEAN DEFAULT false,                               /* The Field has been commanded to the Standby State */
    PreHeat_Field BOOLEAN DEFAULT false,                               /* The Field has been commanded to the Preheat State */
    Generation_Field BOOLEAN DEFAULT false,                            /* The Field has been commanded to the Generation State */
    PostHeat_Field BOOLEAN DEFAULT false,                              /* The Field has been commanded to the Postheat State */
    Defocus_Field BOOLEAN DEFAULT false,                               /* The Field has been commanded to the Emergency Defocus State */
    Sleep_Field BOOLEAN DEFAULT false,                                 /* The Field has been commanded to the Night Stow State */
    RainWash_Field BOOLEAN DEFAULT false,                              /* The Field has been commanded to the Rainwash State */
    Defrost_Field BOOLEAN DEFAULT false,                               /* The Field has been commanded to the Defrost State */
    OperatorControl_Field BOOLEAN DEFAULT false,                       /* The Field has been commanded to the Operator Control Mode */
    AutomaticControl_Field BOOLEAN DEFAULT false,                      /* The Field has been commanded to the Automatic Control Mode */
    AnalystControl_Field BOOLEAN DEFAULT false,                        /* The Field has been commanded to the Analyst Control Mode */
  State_Field INTEGER DEFAULT 0,                                       /* The actual state of the field */
    Stowing_Field BOOLEAN DEFAULT false,                               /* The Field is transitioning to the Wind Stow State */
    Stowed_Field BOOLEAN DEFAULT false,                                /* The Field has transitioned to the Wind Stow State */
    ToStandBy_Field BOOLEAN DEFAULT false,                             /* The Field is transitioning to the Standby State */
    AtStandBy_Field BOOLEAN DEFAULT false,                             /* The Field has transitioned to the Standby State */
    PreHeating_Field BOOLEAN DEFAULT false,                            /* The Field is transitioning to the Preheat State */
    PreHeated_Field BOOLEAN DEFAULT false,                             /* The Field has transitioned to the Preheat State */
    GoingToGeneration_Field BOOLEAN DEFAULT false,                     /* The Field is transitioning to the Generation State */
    Generating_Field BOOLEAN DEFAULT false,                            /* The Field has transitioned to the Generation State */
    ToPostHeat_Field BOOLEAN DEFAULT false,                            /* The Field is transitioning to the Postheat State */
    AtPostHeat_Field BOOLEAN DEFAULT false,                            /* The Field has transitioned to the Postheat State */
    Defocusing_Field BOOLEAN DEFAULT false,                            /* The Field is transitioning to the Emergency Defocus State */
    Defocused_Field BOOLEAN DEFAULT false,                             /* The Field has transitioned to the Emergency Defocus State */
    GoingToSleep_Field BOOLEAN DEFAULT false,                          /* The Field is transitioning to the Night Stow State */
    Sleeping_Field BOOLEAN DEFAULT false,                              /* The Field has transitioned to the Night Stow State */
    ToRainWash_Field BOOLEAN DEFAULT false,                            /* The Field is transitioning to the Rainwash State */
    AtRainWash_Field BOOLEAN DEFAULT false,                            /* The Field has transitioned to the Rainwash State */
    ToDefrost_Field BOOLEAN DEFAULT false,                             /* The Field is transitioning to the Defrost State */
    AtDefrost_Field BOOLEAN DEFAULT false,                             /* The Field has transitioned to the Defrost State */
  FluxSunDNIReport_Field DOUBLE PRECISION DEFAULT 0.0,                 /* The DNI for this report (kw/m^2) */
  FluxSunClearSkyReport_Field DOUBLE PRECISION DEFAULT 0.0,            /* The clear sky DNI for this report (w/m^2) */
  QGrossSolarReport_Field DOUBLE PRECISION DEFAULT 0.0,                /* The total solar energy available to the field for this report (MW-thermal) */
  QLossShadowReport_Field DOUBLE PRECISION DEFAULT 0.0,                /* The shadowing losses in the field for this report (MW-thermal) */
  QLossCosineReport_Field DOUBLE PRECISION DEFAULT 0.0,                /* The cosine losses in the field for this report (MW-thermal) */
  QLossMirrorReport_Field DOUBLE PRECISION DEFAULT 0.0,                /* The mirror reflectivity losses in the field for this report (MW-thermal) */
  QLossOffpointReport_Field DOUBLE PRECISION DEFAULT 0.0,              /* The heliostat off-pointing losses in the field for this report (MW-thermal) */
  QLossBlockingReport_Field DOUBLE PRECISION DEFAULT 0.0,              /* The blocking losses in the field for this report (MW-thermal) */
  QLossAtmosphereReport_Field DOUBLE PRECISION DEFAULT 0.0,            /* The atmospheric losses in the field for this report (MW-thermal) */
  QLossSpillageReport_Field DOUBLE PRECISION DEFAULT 0.0,              /* The spillage losses in the field for this report (MW-thermal) */
  QSpillageEstimated_Field DOUBLE PRECISION DEFAULT 0.0,               /* The spillage losses in the field estimated by FluxCalc (MW-thermal) */
  TimeTagHabReport_Field TIMESTAMPTZ DEFAULT now()                     /* This time the report data was calculated by FluxCalc */
);
  
/* Concentrated Solar Power */
CREATE TABLE Plant(
  Instance_Plant INTEGER PRIMARY KEY,                                  /* Primary Key for the Plant Table */
  ID_Plant VARCHAR(32) DEFAULT '',                                     /* The name of the plant */
  TimeStampSetpoints_Plant VARCHAR(24) DEFAULT '',                     /* The time stamp when the heliostat controls were updated. */
  TimeStampBCS_Plant VARCHAR(24) DEFAULT ''                            /* The time stamp when the BCS correction terms were updated. */
);
  
/* Plant Commanded State */
CREATE TABLE Control(
  Instance_Control INTEGER PRIMARY KEY,                                /* Primary Key for the Control Table */
  ID_Control VARCHAR(32) DEFAULT '',                                   /* The name of the plant commanded state */
  Plant_Control INTEGER DEFAULT 0,                                     /* The commanded control status of the plant */
    Shutdown_Control BOOLEAN DEFAULT false,                            /* Overnight Shutdown */
    StartUp_Control BOOLEAN DEFAULT false,                             /* Stand-by for StartUp */
    PreHeat_Control BOOLEAN DEFAULT false,                             /* Pre-heat for StartUp */
    CollectionStorage_Control BOOLEAN DEFAULT false,                   /* Thermal Energy Collection and Storage */
    CollectionProduction_Control BOOLEAN DEFAULT false,                /* Thermal Energy Collection and Production */
    Production_Control BOOLEAN DEFAULT false,                          /* Thermal Energy Production Only */
    CloudStandByProduction_Control BOOLEAN DEFAULT false,              /* Cloud Stand-by with Production */
    CloudStandBy_Control BOOLEAN DEFAULT false,                        /* Cloud Stand-by with no Production */
    SteamGeneratorStartUp_Control BOOLEAN DEFAULT false,               /* Steam Generation Attemperation StartUp */
    CollectionPumpFailure_Control BOOLEAN DEFAULT false,               /* Thermal Energy Collection with Pump Failure */
    ProductionPumpFailure_Control BOOLEAN DEFAULT false,               /* Thermal Energy Production with Pump Failure */
    FlowLossEmergencyCool_Control BOOLEAN DEFAULT false,               /* Loss of Normal receiver Flow, Emergency cooling */
    TransferHotToCold_Control BOOLEAN DEFAULT false,                   /* Salt Transfer Hot to Cold tank */
    TransferColdToHot_Control BOOLEAN DEFAULT false,                   /* Salt Transfer Cold to Hot tank */
    PurificationHotTank_Control BOOLEAN DEFAULT false,                 /* Salt purification in Hot tank */
    ReducedSteamProduction_Control BOOLEAN DEFAULT false               /* Steam Production at Reduced conditions */
);
  
/* Plant Current State */
CREATE TABLE Status(
  Instance_Status INTEGER PRIMARY KEY,                                 /* Primary Key for the Status Table */
  ID_Status VARCHAR(32) DEFAULT '',                                    /* The name of the plant actual state */
  Plant_Status INTEGER DEFAULT 0,                                      /* The actual control status of the plant */
    Shutdown_Status BOOLEAN DEFAULT false,                             /* Overnight Shutdown */
    StartUp_Status BOOLEAN DEFAULT false,                              /* Stand-by for StartUp */
    PreHeat_Status BOOLEAN DEFAULT false,                              /* Pre-heat for StartUp */
    CollectionStorage_Status BOOLEAN DEFAULT false,                    /* Thermal Energy Collection and Storage */
    CollectionProduction_Status BOOLEAN DEFAULT false,                 /* Thermal Energy Collection and Production */
    Production_Status BOOLEAN DEFAULT false,                           /* Thermal Energy Production Only */
    CloudStandByProduction_Status BOOLEAN DEFAULT false,               /* Cloud Stand-by with Production */
    CloudStandBy_Status BOOLEAN DEFAULT false,                         /* Cloud Stand-by with no Production */
    SteamGeneratorStartUp_Status BOOLEAN DEFAULT false,                /* Steam Generation Attemperation StartUp */
    CollectionPumpFailure_Status BOOLEAN DEFAULT false,                /* Thermal Energy Collection with pump failure */
    ProductionPumpFailure_Status BOOLEAN DEFAULT false,                /* Thermal Energy Production with pump failure */
    FlowLossEmergencyCool_Status BOOLEAN DEFAULT false,                /* Loss of Normal receiver flow, emergency cooling */
    TransferHotToCold_Status BOOLEAN DEFAULT false,                    /* Salt transfer hot to cold tank */
    TransferColdToHot_Status BOOLEAN DEFAULT false,                    /* Salt transfer cold to hot tank */
    PurificationHotTank_Status BOOLEAN DEFAULT false,                  /* Salt purification in hot tank */
    ReducedSteamProduction_Status BOOLEAN DEFAULT false                /* Steam Production at reduced conditions */
);
  
/* Field Default State Records */
CREATE TABLE FieldDefault(
  Instance_FieldDefault INTEGER PRIMARY KEY,                           /* Primary Key for the FieldDefault Table */
  AngleStowElevation_FieldDefault DOUBLE PRECISION DEFAULT 0.0,        /* The Elevation angle to stow the field for DCS loss of comm (degrees) */
  State_FieldDefault INTEGER DEFAULT 0,                                /* The default state for the field upon loss of comm with the DCS */
    Sleep_FieldDefault BOOLEAN DEFAULT false,                          /* The default state is Night Stow for DCS loss of comm. */
    Standby_FieldDefault BOOLEAN DEFAULT false,                        /* The default state is Standby for DCS loss of comm. */
    ElevationKnownStow_FieldDefault BOOLEAN DEFAULT false,             /* The default state is Stow with a specified elevation for DCS loss of comm. */
  TimeTagHab_FieldDefault TIMESTAMPTZ DEFAULT now()                    /* The last time the field went to its default state */
);
  
/* Field aiming target definition */
CREATE TABLE Target(
  Instance_Target INTEGER PRIMARY KEY,                                 /* Primary Key for the Target Table */
  Aim_Target INTEGER DEFAULT 0,                                        /* The aiming target in operation */
    Receiver_Target BOOLEAN DEFAULT false,                             /* The aiming target is the receiver. This is the normal operating model. */
    Sun_Target BOOLEAN DEFAULT false                                   /* The aiming target is the sun. This is only the case in the Defrost state. */
);
  
/* Heliostat aiming tables */
CREATE TABLE Aiming(
  Instance_Aiming INTEGER PRIMARY KEY,                                 /* Primary Key for the Aiming Table */
  ID_Aiming VARCHAR(32) DEFAULT '',                                    /* The name of the aimingtable */
  GainMax_Aiming DOUBLE PRECISION DEFAULT 0.0,                         /* The gain for the aiming adjustment equation at the starting ring */
  GainMin_Aiming DOUBLE PRECISION DEFAULT 0.0,                         /* The gain for the aiming adjustment equation at the last ring */
  ExponentDecay_Aiming DOUBLE PRECISION DEFAULT 0.0,                   /* The power law adjustment of the aiming */
  TimeStampStart_Aiming VARCHAR(24) DEFAULT '',                        /* The starting date for the aiming table */
  ReflectivityMirror_Aiming DOUBLE PRECISION DEFAULT 0.0,              /* The heliostat mirror reflectivity on which the aiming tables are based (fraction) */
  SoilingMirror_Aiming DOUBLE PRECISION DEFAULT 0.0,                   /* The heliostat mirror degradation from soiling on which the aiming tables are based (fraction) */
  ReflectivityReceiver_Aiming DOUBLE PRECISION DEFAULT 0.0,            /* The receiver panel reflectivity on which the aiming tables are based (fraction) */
  EmissivityReceiver_Aiming DOUBLE PRECISION DEFAULT 0.0,              /* The receiver panel emissivity on which the aiming tables are based (fraction) */
  SpeedWind_Aiming DOUBLE PRECISION DEFAULT 0.0,                       /* The field windspeed on which aiming tables are based (m/s) */
  VisibilityAtmospheric_Aiming DOUBLE PRECISION DEFAULT 0.0,           /* The atmospheric visibility on which the aiming tables are based (km) */
  PctStrainMax_Aiming DOUBLE PRECISION DEFAULT 0.0,                    /* The Pct Tube Strain on which the aiming tables were based (Percent) */
  TempInnerWallMax_Aiming DOUBLE PRECISION DEFAULT 0.0,                /* The maximum inner wall temperature limit on which the aiming tables were based (deg-F) */
  TempCInnerWallMax_Aiming DOUBLE PRECISION DEFAULT 0.0,               /* The maximum inner wall temperature limit on which the aiming tables were based (deg-C) */
  QheatShieldMax_Aiming DOUBLE PRECISION DEFAULT 0.0,                  /* The maximum heat shield flux limit on which the aiming tables wer based (kw/m^2) */
  TempInlet_Aiming DOUBLE PRECISION DEFAULT 0.0,                       /* The inlet temperature on which the aiming tables were based (deg-F) */
  TempOutlet_Aiming DOUBLE PRECISION DEFAULT 0.0,                      /* The outlet temperature on which the aiming tables were based (deg-F) */
  TempMinStorage_Aiming DOUBLE PRECISION DEFAULT 0.0,                  /* The minimum temperature for sending the outlet to storage (deg-F) */
  TempCInlet_Aiming DOUBLE PRECISION DEFAULT 0.0,                      /* The inlet temperature on which the aiming tables were based (deg-C) */
  TempCOutlet_Aiming DOUBLE PRECISION DEFAULT 0.0,                     /* The outlet temperature on which the aiming tables were based (deg-C) */
  TempCMinStorage_Aiming DOUBLE PRECISION DEFAULT 0.0,                 /* The minimum temperature for sending the outlet to storage (deg-C) */
  WflowMin_Aiming DOUBLE PRECISION DEFAULT 0.0,                        /* The receiver minimum flow rate limit on which the aiming tables were based (Lbm/s) */
  WflowMax_Aiming DOUBLE PRECISION DEFAULT 0.0,                        /* The receiver maximum flow rate limit on which the aiming tables were based (Lbm/s) */
  QflowMax_Aiming DOUBLE PRECISION DEFAULT 0.0,                        /* The receiver thermal rate limit correcsponding to WflowMax (MWt) */
  QPreheat_Aiming DOUBLE PRECISION DEFAULT 0.0                         /* The nominal preheat aiming power (MWt) */
);
  
/* Day entries for the aiming tables */
CREATE TABLE Aim(
  Instance_Aim INTEGER PRIMARY KEY,                                    /* Primary Key for the Aim Table */
  Day_Aim INTEGER DEFAULT 0,                                           /* The day past the starting date for the aiming */
  TimeStampDay_Aim VARCHAR(24) DEFAULT '',                             /* The time stamp for the aiming day */
  P$AimTime_Aim INTEGER DEFAULT 0                                      /* Pointer to the aiming time intervals before and after solar noon for the aiming */
);
  
/* Hourly time entryies for the aiming table days */
CREATE TABLE AimTime(
  Instance_AimTime INTEGER PRIMARY KEY,                                /* Primary Key for the AimTime Table */
  PctRiseToNoon_AimTime DOUBLE PRECISION DEFAULT 0.0,                  /* Percent of time between sunrise and solar noon (percent) */
  MinutesToNoon_AimTime DOUBLE PRECISION DEFAULT 0.0,                  /* Minutes to solar noon (minutes) */
  dTime_AimTime INTEGER DEFAULT 0,                                     /* Time to solar noon for this aim (10^-7 seconds) */
  DNIClearSky_AimTime DOUBLE PRECISION DEFAULT 0.0,                    /* The clear sky DNI for this time (w/m^2) */
  P$AimLevel_AimTime INTEGER DEFAULT 0                                 /* Pointer to the DNI levels where separate aimpoints were calculated */
);
  
/* Aimpoint DNI levels */
CREATE TABLE AimLevel(
  Instance_AimLevel INTEGER PRIMARY KEY,                               /* Primary Key for the AimLevel Table */
  FluxDNI_AimLevel DOUBLE PRECISION DEFAULT 0.0,                       /* The DNI for this aiming level (kw/m^2) */
  Qincident_AimLevel DOUBLE PRECISION DEFAULT 0.0,                     /* The solar power incident on the field  (MW-thermal) */
  QGrossSolar_AimLevel DOUBLE PRECISION DEFAULT 0.0,                   /* The total solar energy available to the field for this aiming level (MW-thermal) */
  QLossShadowing_AimLevel DOUBLE PRECISION DEFAULT 0.0,                /* The shadowing losses in the field for this aiming level (MW-thermal) */
  QLossCosine_AimLevel DOUBLE PRECISION DEFAULT 0.0,                   /* The cosine losses in the field for this aiming level (MW-thermal) */
  QlossMirror_AimLevel DOUBLE PRECISION DEFAULT 0.0,                   /* The mirror reflectivity losses in the field for this aiming level (MW-thermal) */
  QlossOffpointing_AimLevel DOUBLE PRECISION DEFAULT 0.0,              /* The heliostat off-pointing losses in the field for this aiming level (MW-thermal) */
  QlossBlocking_AimLevel DOUBLE PRECISION DEFAULT 0.0,                 /* The blocking losses in the field for this aiming level (MW-thermal) */
  QlossAtmospheric_AimLevel DOUBLE PRECISION DEFAULT 0.0,              /* The atmospheric losses in the field for this aiming level (MW-thermal) */
  QlossSpillage_AimLevel DOUBLE PRECISION DEFAULT 0.0,                 /* The spillage losses in the field for this aiming level (MW-thermal) */
  QReceiverIncident_AimLevel DOUBLE PRECISION DEFAULT 0.0,             /* The receover incident thermal power for this aiming (MW-Thermal */
  QlossThermal_AimLevel DOUBLE PRECISION DEFAULT 0.0,                  /* The thermal (radiation and conductivity) losses for this aiming (MW-Thermal) */
  Qthermal_AimLevel DOUBLE PRECISION DEFAULT 0.0,                      /* The thermal power produced for this aiming (MW-Thermal) */
  WFlow_AimLevel DOUBLE PRECISION DEFAULT 0.0,                         /* The flow rate for this aiming level(lbm/s) */
  P$AimPoint_AimLevel INTEGER DEFAULT 0                                /* Pointer to the aimpoints at the different DNI levels */
);
  
/* Aiming setpoints */
CREATE TABLE AimPoint(
  Instance_AimPoint INTEGER PRIMARY KEY,                               /* Primary Key for the AimPoint Table */
  Adjust_AimPoint DOUBLE PRECISION DEFAULT 0.0,                        /* The Gain adjustment to Zaim for a wedge for maximum power at this aimpoint (multiplier) */
  Retain_AimPoint DOUBLE PRECISION DEFAULT 0.0,                        /* The heliostats retained in a wedge for maximum power at this aimpoint (fraction) */
  QIncidentPanel_AimPoint DOUBLE PRECISION DEFAULT 0.0,                /* The power incident on a wedge at this aimpoint (MW-Thermal) */
  RetainPreheat_AimPoint DOUBLE PRECISION DEFAULT 0.0                  /* The heliostats retained in a wedge for preheat at this aimpoint (fraction) */
);
  
/* Infrared Camera */
CREATE TABLE IRCamera(
  Instance_IRCamera INTEGER PRIMARY KEY,                               /* Primary Key for the IRCamera Table */
  ID_IRCamera VARCHAR(32) DEFAULT '',                                  /* The name of a infrared camera */
  Name_IRCamera VARCHAR(16) DEFAULT '',                                /* The short name of a infrared camera */
  X_IRCamera DOUBLE PRECISION DEFAULT 0.0,                             /* The ENU East coordinate of the infrared camera (m) */
  Y_IRCamera DOUBLE PRECISION DEFAULT 0.0,                             /* The ENU North coordinate of the infrared camera (m) */
  Z_IRCamera DOUBLE PRECISION DEFAULT 0.0,                             /* The ENU Up coordinate of the infrared camera (m) */
  Latitude_IRCamera DOUBLE PRECISION DEFAULT 0.0,                      /* The surveyed latitude of the infrared camera (degrees) */
  Longitude_IRCamera DOUBLE PRECISION DEFAULT 0.0,                     /* The surveyed longitude of the infrared camera (degrees) */
  Elevation_IRCamera DOUBLE PRECISION DEFAULT 0.0,                     /* The surveyed elevation of the infrared camera (m) */
  XLocation_IRCamera DOUBLE PRECISION DEFAULT 0.0,                     /* The ENU East coordinate of the infrared camera foundation (m) */
  YLocation_IRCamera DOUBLE PRECISION DEFAULT 0.0,                     /* The ENU North coordinate of the infrared camera foundation (m) */
  ZLocation_IRCamera DOUBLE PRECISION DEFAULT 0.0,                     /* The ENU Up coordinate of the infrared camera foundation (m) */
  dLatitude_IRCamera DOUBLE PRECISION DEFAULT 0.0,                     /* The surveyed Northing coordinate of the IR Camera foundation (us survey feet) */
  dLongitude_IRCamera DOUBLE PRECISION DEFAULT 0.0,                    /* The surveyed Easting coordinate of the IR Camera foundation (us survey feet) */
  ElevationPTU_IRCamera DOUBLE PRECISION DEFAULT 0.0,                  /* The elevation above ground level of the pivot point of the Pan/Tilt Unit (m) */
  HeightVerticalPTU_IRCamera DOUBLE PRECISION DEFAULT 0.0,             /* The height of the pan/tilt unit when it is oriented vertically (m) */
  HeightCamera_IRCamera DOUBLE PRECISION DEFAULT 0.0,                  /* The Height of the camera (m) */
  XOffsetCamera_IRCamera DOUBLE PRECISION DEFAULT 0.0,                 /* The Horizontal Offset of the camera from the PTU pivot point (m) */
  YOffsetCamera_IRCamera DOUBLE PRECISION DEFAULT 0.0,                 /* The Normal Offset of the camera from the PTU pivot point (m) */
  ZOffsetCamera_IRCamera DOUBLE PRECISION DEFAULT 0.0,                 /* The Vertical Offset of the camera from the PTU pivot point (m) */
  AngleOrientationPTU_IRCamera DOUBLE PRECISION DEFAULT 0.0,           /* The orientation of the pan/tilt unit from vertical (degrees) */
  AngleAzimuthalPTU_IRCamera DOUBLE PRECISION DEFAULT 0.0,             /* The azimuthal rotation of the pan/tilt unit (degrees) */
  AngleElevationPTU_IRCamera DOUBLE PRECISION DEFAULT 0.0,             /* The elevation rotation of the pan/tilt unit (degrees */
  Xtarget_IRCamera DOUBLE PRECISION DEFAULT 0.0,                       /* The ENU East coordinate of the infrared camera focal point (m) */
  Ytarget_IRCamera DOUBLE PRECISION DEFAULT 0.0,                       /* The ENU North coordinate of the infrared camera focal point (m) */
  Ztarget_IRCamera DOUBLE PRECISION DEFAULT 0.0,                       /* The ENU Up coordinate of the infrared camera focal point (m) */
  PanTilt$Switch_IRCamera INTEGER DEFAULT 0,                           /* The pointer to the pan/tilt unit for the IRCamera */
  Panel$Switch_IRCamera INTEGER DEFAULT 0,                             /* The pointer to the field termination panel for the IRCamera */
  Camera$Video_IRCamera INTEGER DEFAULT 0,                             /* The pointer to the video camera instance of the IRCamera */
  Foundation$HFC_IRCamera INTEGER DEFAULT 0,                           /* The pointer to the Ethernet HFC device */
  CommandCamera_IRCamera VARCHAR(128) DEFAULT '',                      /* The Command to the camera */
  CommandCameraRecord_IRCamera VARCHAR(128) DEFAULT '',                /* The record of the last command to the camera */
  TimeTagCamera_IRCamera TIMESTAMPTZ DEFAULT now(),                    /* The time tag for the camera command */
  Address_IRCamera VARCHAR(32) DEFAULT '',                             /* The TCP/IP Address of the IR Camera */
  Equipment_IRCamera VARCHAR(16) DEFAULT '',                           /* The Equipment tag for this camera */
  P$IRImage_IRCamera INTEGER DEFAULT 0,                                /* The children images of the IR camera */
  TempFMax_IRCamera DOUBLE PRECISION DEFAULT 0.0,                      /* The maximum temperature from the IR camera (degF) */
  TempCMax_IRCamera DOUBLE PRECISION DEFAULT 0.0,                      /* The maximum temperature from the IR camera (degC) */
  TempFMin_IRCamera DOUBLE PRECISION DEFAULT 0.0,                      /* The minimum temperature from the IR camera (degF) */
  TempCMin_IRCamera DOUBLE PRECISION DEFAULT 0.0,                      /* The minimum temperature from the IR camera (degC) */
  StatusTemperature_IRCamera INTEGER DEFAULT 0,                        /* The status of the IR camera temperature measurment */
    Normal_IRCamera BOOLEAN DEFAULT false,                             /* The measurement is good */
    Fault_IRCamera BOOLEAN DEFAULT false,                              /* The measurement is bad */
    Replaced_IRCamera BOOLEAN DEFAULT false,                           /* The measurement has been manually replaced */
  TempFMaxHeatShield_IRCamera DOUBLE PRECISION DEFAULT 0.0,            /* The maximum heatshield temperature from the IR camera (degF) */
  TempFMxHeatShieldUpper_IRCamera DOUBLE PRECISION DEFAULT 0.0,        /* The maximum upper heatshield temperature from the IR camera (degF) */
  TempFMxHeatShieldLower_IRCamera DOUBLE PRECISION DEFAULT 0.0,        /* The maximum lower heatshield temperature from the IR camera (degF) */
  TempCMaxHeatShield_IRCamera DOUBLE PRECISION DEFAULT 0.0,            /* The maximum heatshield temperature from the IR camera (degC) */
  TempCMxHeatShieldUpper_IRCamera DOUBLE PRECISION DEFAULT 0.0,        /* The maximum upper heatshield temperature from the IR camera (degC) */
  TempCMxHeatShieldLower_IRCamera DOUBLE PRECISION DEFAULT 0.0,        /* The maximum lower heat shield temperature from the IR camera (degC) */
  StatusHeatShieldUpper_IRCamera INTEGER DEFAULT 0,                    /* The status of the IR upper heatshield measurement */
    NormalUpper_IRCamera BOOLEAN DEFAULT false,                        /* The measurement is good */
    FaultUpper_IRCamera BOOLEAN DEFAULT false,                         /* The measurement is bad */
    ReplacedUpper_IRCamera BOOLEAN DEFAULT false,                      /* The measurement has been manually replaced */
  StatusHeatShieldLower_IRCamera INTEGER DEFAULT 0,                    /* The status of the IR lower heatshield measurement */
    NormalLower_IRCamera BOOLEAN DEFAULT false,                        /* The measurement is good */
    FaultLower_IRCamera BOOLEAN DEFAULT false,                         /* The measurement is bad */
    ReplacedLower_IRCamera BOOLEAN DEFAULT false,                      /* The measurement has been manually replaced */
  TimeTagTemperature_IRCamera TIMESTAMPTZ DEFAULT now(),               /* The time tag for the last successful update of the data */
  StatusViolation_IRCamera INTEGER DEFAULT 0,                          /* The temperature violation status from the IR camera */
    NoViolation_IRCamera BOOLEAN DEFAULT false,                        /* The temperature is below the warning limit */
    WarningViolation_IRCamera BOOLEAN DEFAULT false,                   /* The temperature is below the trip limit but above the warning limit */
    TripViolation_IRCamera BOOLEAN DEFAULT false,                      /* The temperature is above the trip limit */
  State_IRCamera INTEGER DEFAULT 0,                                    /* The UI Operator selected state of the IR Camera */
    Online_IRCamera BOOLEAN DEFAULT false,                             /* The IR Camera is online */
    Offline_IRCamera BOOLEAN DEFAULT false                             /* The IR Camera is offline */
);
  
/* Infrared Camera Data */
CREATE TABLE IRImage(
  Instance_IRImage INTEGER PRIMARY KEY,                                /* Primary Key for the IRImage Table */
  TimeTagHab_IRImage TIMESTAMPTZ DEFAULT now(),                        /* The time when the image as taken */
  P$IRCamera_IRImage INTEGER DEFAULT 0                                 /* The parent camera of the image */
);
  
/* BCS data */
CREATE TABLE BCS(
  Instance_BCS INTEGER PRIMARY KEY,                                    /* Primary Key for the BCS Table */
  NumHeliostatsPosition_BCS INTEGER DEFAULT 0,                         /* Number of heliostats being processed by the BCS Camera */
  NumHeliostatsAtPosition_BCS INTEGER DEFAULT 0,                       /* Number of heliostats being imaged by the BCS by the BCS Camera */
  ID_BCS VARCHAR(32) DEFAULT '',                                       /* The name of a beam characterization system */
  Type_BCS INTEGER DEFAULT 0,                                          /* The type of BCS Target */
    Tower_BCS BOOLEAN DEFAULT false,                                   /* The BCS Target is fixed to the tower. */
    Portable_BCS BOOLEAN DEFAULT false,                                /* The BCS Target is portable */
  XCamera_BCS DOUBLE PRECISION DEFAULT 0.0,                            /* The ENU East coordinate of the BCS camera (m) */
  YCamera_BCS DOUBLE PRECISION DEFAULT 0.0,                            /* The ENU North coordinate of the BCS camera (m) */
  ZCamera_BCS DOUBLE PRECISION DEFAULT 0.0,                            /* The ENU Up coordinate of the BCS camera (m) */
  Latitude_BCS DOUBLE PRECISION DEFAULT 0.0,                           /* The surveyed latitude of the BCS camera (degrees) */
  Longitude_BCS DOUBLE PRECISION DEFAULT 0.0,                          /* The surveyed longitude of the BCS camera (degrees) */
  Elevation_BCS DOUBLE PRECISION DEFAULT 0.0,                          /* The surveyed elevation of the BCS camera (m) */
  XLocation_BCS DOUBLE PRECISION DEFAULT 0.0,                          /* The ENU East coordinate of the BCS camera foundation (m) */
  YLocation_BCS DOUBLE PRECISION DEFAULT 0.0,                          /* The ENU North coordinate of the BCS camera foundation (m) */
  ZLocation_BCS DOUBLE PRECISION DEFAULT 0.0,                          /* The ENU Up coordinate of the BCS camera foundation (m) */
  dLatitude_BCS DOUBLE PRECISION DEFAULT 0.0,                          /* The surveyed Northing coordinate of the BCS Camera foundation (us survey feet) */
  dLongitude_BCS DOUBLE PRECISION DEFAULT 0.0,                         /* The surveyed Easting coordinate of the BCS Camera foundation (us survey feet) */
  ElevationPTU_BCS DOUBLE PRECISION DEFAULT 0.0,                       /* The elevation above ground level of the pivot point of the Pan/Tilt Unit (m) */
  HeightVerticalPTU_BCS DOUBLE PRECISION DEFAULT 0.0,                  /* The height of the pan/tilt unit when it is oriented vertically (m) */
  HeightCamera_BCS DOUBLE PRECISION DEFAULT 0.0,                       /* The Height of the camera (m) */
  XOffsetCamera_BCS DOUBLE PRECISION DEFAULT 0.0,                      /* The Horizontal Offset of the camera from the PTU pivot point (m) */
  YOffsetCamera_BCS DOUBLE PRECISION DEFAULT 0.0,                      /* The Normal Offset of the camera from the PTU pivot point (m) */
  ZOffsetCamera_BCS DOUBLE PRECISION DEFAULT 0.0,                      /* The Vertical Offset of the camera from the PTU pivot point (m) */
  AngleOrientationPTU_BCS DOUBLE PRECISION DEFAULT 0.0,                /* The orientation of the pan/tilt unit from vertical (degrees) */
  AngleAzimuthalPTU_BCS DOUBLE PRECISION DEFAULT 0.0,                  /* The azimuthal rotation of the pan/tilt unit (degrees) */
  AngleElevationPTU_BCS DOUBLE PRECISION DEFAULT 0.0,                  /* The elevation rotation of the pan/tilt unit (degrees */
  Address_BCS VARCHAR(32) DEFAULT '',                                  /* The TCP/IP Address of the BCS Camera */
  AddressPTU_BCS VARCHAR(32) DEFAULT '',                               /* The TCP/IP Address of the BCS Camera Pan/Tilt Unit */
  Equipment_BCS VARCHAR(16) DEFAULT '',                                /* The Equipment tag for this camera */
  XTarget_BCS DOUBLE PRECISION DEFAULT 0.0,                            /* The ENU East coordinate of the BCS Target (m) */
  YTarget_BCS DOUBLE PRECISION DEFAULT 0.0,                            /* The ENU North coordinate of the BCS Target (m) */
  ZTarget_BCS DOUBLE PRECISION DEFAULT 0.0,                            /* The ENU Up coordinate of the BCS Target (m) */
  HeightTarget_BCS DOUBLE PRECISION DEFAULT 0.0,                       /* The height of the target (m) */
  WidthTarget_BCS DOUBLE PRECISION DEFAULT 0.0,                        /* The width of the target (m) */
  ElevationTarget_BCS DOUBLE PRECISION DEFAULT 0.0,                    /* The elevation of the BCS target above ground (m) */
  PanTilt$Switch_BCS INTEGER DEFAULT 0,                                /* The pointer to the pan/tilt unit for the BCS */
  Panel$Switch_BCS INTEGER DEFAULT 0,                                  /* The pointer to the field termination panel for the BCS */
  Camera$Video_BCS INTEGER DEFAULT 0,                                  /* The pointer to the video camera for the BCS */
  Foundation$HFC_BCS INTEGER DEFAULT 0,                                /* The pointer to the Ethernet HFC device */
  CommandCamera_BCS VARCHAR(128) DEFAULT '',                           /* The Command to the camera */
  CommandCameraRecord_BCS VARCHAR(128) DEFAULT '',                     /* The record of the last command to the camera */
  TimeTagCamera_BCS TIMESTAMPTZ DEFAULT now(),                         /* The time tag for the camera command */
  EncoderAzCmd_BCS INTEGER DEFAULT 0,                                  /* The commanded azimuthal encoder position to focus on the BCS Target (counts) */
  EncoderElCmd_BCS INTEGER DEFAULT 0,                                  /* The commanded elevation encoder position to focus on the BCS Target (counts) */
  I$Heliostat_BCS INTEGER DEFAULT 0,                                   /* The heliostat currently being processed by the BCS */
  StatusSet_BCS INTEGER DEFAULT 0,                                     /* The commanded mode of the BCS */
    OnlineSet_BCS BOOLEAN DEFAULT false,                               /* The BCS is under automatic control */
    OfflineSet_BCS BOOLEAN DEFAULT false,                              /* The BCS is under manual control */
  TimeTagLastGoodRegistration_BCS TIMESTAMPTZ DEFAULT now(),           /* The time that the last good registration image was processed for this camera */
  TimeTagRegistration_BCS TIMESTAMPTZ DEFAULT now(),                   /* The time that the last registration image was processed for this camera */
  IntensityStandardDeviation_BCS DOUBLE PRECISION DEFAULT 0.0,         /* The standard deviation of the intensity on the last registration image */
  StatusRegistration_BCS INTEGER DEFAULT 0,                            /* The status of the BCS Registration Image */
    Unknown_BCS BOOLEAN DEFAULT false,                                 /* The registration status could not be determined */
    Good_BCS BOOLEAN DEFAULT false,                                    /* The registration completed successfully */
    Failure_BCS BOOLEAN DEFAULT false,                                 /* The registration failed */
    NoImageTaken_BCS BOOLEAN DEFAULT false,                            /* The image was not taken */
    FileDoesNotExist_BCS BOOLEAN DEFAULT false,                        /* The image file was not found */
    FileIsNotABitMap_BCS BOOLEAN DEFAULT false,                        /* The image file was not a bitmap */
    BitMapWidthInvalid_BCS BOOLEAN DEFAULT false,                      /* The image bitmap width was invalid */
    BitMapHeightInvalid_BCS BOOLEAN DEFAULT false,                     /* The image bitmap height was invalid */
    BitMapSizeInfoInvalid_BCS BOOLEAN DEFAULT false,                   /* The image information size was not 40 */
    InvalidFileName_BCS BOOLEAN DEFAULT false,                         /* The image file name was invalid */
    InvalidFileTimeTag_BCS BOOLEAN DEFAULT false,                      /* The image file time created was invalid */
    UnableToModifyFileTime_BCS BOOLEAN DEFAULT false,                  /* The image file time could not be modified */
    FileOpenFailure_BCS BOOLEAN DEFAULT false,                         /* The image file could not be opened */
    FileReadFailure_BCS BOOLEAN DEFAULT false,                         /* An error occurred reading the file */
    FileWriteFailure_BCS BOOLEAN DEFAULT false,                        /* An error occurred updating the file */
    DegenerateLinearEstimate_BCS BOOLEAN DEFAULT false,                /* The linear estimate of the target bottom failed */
    ScaleTestFailure_BCS BOOLEAN DEFAULT false,                        /* The image edge length ratio test failed */
    StarkRatioTestFailure_BCS BOOLEAN DEFAULT false,                   /* The image dark to light pixel ratio test failed */
    CornersShiftFailure_BCS BOOLEAN DEFAULT false,                     /* The image corner locations shifted too much */
    AngleDeviationFailure_BCS BOOLEAN DEFAULT false,                   /* The image corner angle test failed */
    AreaRatioTestFailure_BCS BOOLEAN DEFAULT false,                    /* The image area to target ratio test failed */
    ExcessSpillageOnTarget_BCS BOOLEAN DEFAULT false,                  /* The image had too much spillage */
  Auto$Heliostat_BCS INTEGER DEFAULT 0,                                /* 1st Heliostat eligible for Automatic BCS for this Camera */
  State_BCS INTEGER DEFAULT 0,                                         /* The UI Operator selected state of the BCS camera */
    Online_BCS BOOLEAN DEFAULT false,                                  /* The BCS is online */
    Offline_BCS BOOLEAN DEFAULT false,                                 /* The BCS is offline */
    ScheduleInnerRingHeliostats_BCS BOOLEAN DEFAULT false,             /* Schedule only inner ring heliostats for this BCS.  (The operators set this on cloudy days) */
    ScheduleOuterRingHeliostats_BCS BOOLEAN DEFAULT false,             /* Schedule only outer ring heliostats for this BCS   (The operators set this on clear days) */
    ScheduleAllHeliostats_BCS BOOLEAN DEFAULT false,                   /* Schedule any heliostat for this BCS */
    ScheduleToVerifyHeliostats_BCS BOOLEAN DEFAULT false               /* Schedule to verify pointing accuracy. */
);
  
/* BCS sub-image data */
CREATE TABLE BCSImage(
  Instance_BCSImage INTEGER PRIMARY KEY,                               /* Primary Key for the BCSImage Table */
  XOffset_BCSImage DOUBLE PRECISION DEFAULT 0.0,                       /* The x-offset within the global camera image */
  YOffset_BCSImage DOUBLE PRECISION DEFAULT 0.0,                       /* The y-offset within the global camera image */
  I$Heliostat_BCSImage INTEGER DEFAULT 0,                              /* The heliostat that created the image */
  XCentroid_BCSImage DOUBLE PRECISION DEFAULT 0.0,                     /* The horizontal coordinate of the centroid in the image coordinate system (m) */
  YCentroid_BCSImage DOUBLE PRECISION DEFAULT 0.0                      /* The vertical oordinates of the centroid in the image coordinate system (m) */
);
  
/* BCS Processed Data history (52 per heliostat) */
CREATE TABLE BCSHistory(
  Instance_BCSHistory INTEGER PRIMARY KEY,                             /* Primary Key for the BCSHistory Table */
  TimeTagHAB_BCSHistory TIMESTAMPTZ DEFAULT now(),                     /* The time tag for this BCS data collection point */
  JDT_BCSHistory DOUBLE PRECISION DEFAULT 0.0,                         /* The Julian Date for this BCS data collection point (hours) */
  XTarget_BCSHistory DOUBLE PRECISION DEFAULT 0.0,                     /* The ENU East coordinate where the heliostat was aimed (m) */
  YTarget_BCSHistory DOUBLE PRECISION DEFAULT 0.0,                     /* The ENU North coordinate where the heliostat was aimed (m) */
  ZTarget_BCSHistory DOUBLE PRECISION DEFAULT 0.0,                     /* The ENU Up coordinate where the heliostat was aimed (m) */
  AngleAzCmdDeg_BCSHistory DOUBLE PRECISION DEFAULT 0.0,               /* The heliostat azimuthal commanded angle (degrees) */
  AngleElCmdDeg_BCSHistory DOUBLE PRECISION DEFAULT 0.0,               /* The heliostat elevation commanded angle (degrees) */
  EncoderAzCmd_BCSHistory INTEGER DEFAULT 0,                           /* The heliostat azimuthal commanded encoder position (counts) */
  EncoderElCmd_BCSHistory INTEGER DEFAULT 0,                           /* The heliostat elevation commanded encoder position (counts) */
  Xcentroid_BCSHistory DOUBLE PRECISION DEFAULT 0.0,                   /* The Horizotal distance from the target center of the image centroid(m) */
  Ycentroid_BCSHistory DOUBLE PRECISION DEFAULT 0.0,                   /* The Vertical distance from the target center of the image centroid(m) */
  AngleSunAzimuthal_BCSHistory DOUBLE PRECISION DEFAULT 0.0,           /* The sun azimuthal angle when the image was taken(degrees) */
  AngleSunElevation_BCSHistory DOUBLE PRECISION DEFAULT 0.0,           /* The sun elevation angle when the image was taken(degrees) */
  dAngleElAiming_BCSHistory DOUBLE PRECISION DEFAULT 0.0,              /* Difference between the calculated and measured azimuthal angle (degrees) */
  dAngleAzAiming_BCSHistory DOUBLE PRECISION DEFAULT 0.0,              /* Difference between the calculated and measured azimuthal angle (degrees) */
  SigmaSlopeX_BCSHistory DOUBLE PRECISION DEFAULT 0.0,                 /* The standard deviation of the mirror horizontal slope error (radians) */
  SigmaSlopeY_BCSHistory DOUBLE PRECISION DEFAULT 0.0,                 /* The standard deviation of the mirror vertical slope error (radians) */
  AngleBiasAzimuthal_BCSHistory DOUBLE PRECISION DEFAULT 0.0,          /* Difference between the ideal and measured azimuthal angle (degrees) */
  AngleBiasElevation_BCSHistory DOUBLE PRECISION DEFAULT 0.0,          /* Difference between the ideal and measured elevation angle (degrees) */
  State_BCSHistory INTEGER DEFAULT 0,                                  /* The type of BCS image taken */
    Camera_BCSHistory BOOLEAN DEFAULT false,                           /* A camera image was taken for this history */
    Manual_BCSHistory BOOLEAN DEFAULT false,                           /* A manual BCS capture was taken for this history */
    LaserFocusOnTarget_BCSHistory BOOLEAN DEFAULT false,               /* A nighttime laser focus from the heliostat was manually captured */
    SkipProcessing_BCSHistory BOOLEAN DEFAULT false,                   /* The operator has removed this history data from the estimation. */
    NewEntry_BCSHistory BOOLEAN DEFAULT false,                         /* This is a new entry that has not been included in the estimation */
    RegistrationMismatch_BCSHistory BOOLEAN DEFAULT false,             /* This entry did not have a successfull registration image to use */
    UsePriorRegistration_BCSHistory BOOLEAN DEFAULT false,             /* Not currently used */
    UseLastRegistration_BCSHistory BOOLEAN DEFAULT false,              /* Not currently used */
    UseNextRegistration_BCSHistory BOOLEAN DEFAULT false,              /* Not currently used */
    RayTraceAccepted_BCSHistory BOOLEAN DEFAULT false,                 /* Not currently used */
    RayTraceRejected_BCSHistory BOOLEAN DEFAULT false,                 /* Not currently used */
    EllipseFitAccepted_BCSHistory BOOLEAN DEFAULT false,               /* The Ellipse Fit for the image outline was successful */
    EllipseFitRejected_BCSHistory BOOLEAN DEFAULT false,               /* The Ellipse Fit for the image outline was successful */
    OperatorCentroid_BCSHistory BOOLEAN DEFAULT false,                 /* The Operator manually entered the centroid coordinates */
    OperatorAssisted_BCSHistory BOOLEAN DEFAULT false,                 /* No longer user */
    SingleCentroid_BCSHistory BOOLEAN DEFAULT false,                   /* The image processing assumed a single centroid */
    MultiCentroid_BCSHistory BOOLEAN DEFAULT false,                    /* The image processing picked the most likely centroid */
    StreaksRegistration_BCSHistory BOOLEAN DEFAULT false,              /* Excessive streaking found on the registration image */
    StreaksOnImage_BCSHistory BOOLEAN DEFAULT false,                   /* Excessive streaking found on the heliostat image */
    BiasSaved_BCSHistory BOOLEAN DEFAULT false,                        /* Not currently used */
    TimeTagExact_BCSHistory BOOLEAN DEFAULT false,                     /* This BCSHistory was taken after a camera time tag error was detected */
    MoonFocusOnTarget_BCSHistory BOOLEAN DEFAULT false,                /* This image was captured at night focusing moonlight on the target */
    VenusFocusOnTarget_BCSHistory BOOLEAN DEFAULT false,               /* Future feature */
    MarsFocusOnTarget_BCSHistory BOOLEAN DEFAULT false,                /* Future feature */
    JupiterFocusOnTarget_BCSHistory BOOLEAN DEFAULT false,             /* Future feature */
    StarFocusOnTarget_BCSHistory BOOLEAN DEFAULT false,                /* Future feature */
    PoorImageQuality_BCSHistory BOOLEAN DEFAULT false,                 /* Not currently used */
    FocusEllipseCenter_BCSHistory BOOLEAN DEFAULT false,               /* The geometric center of the ellipse fitted to the image outline was used */
    FocusEllipseCentroid_BCSHistory BOOLEAN DEFAULT false,             /* The centroid of the ellipse fitted to the image outline was used */
  P$Heliostat_BCSHistory INTEGER DEFAULT 0                             /* Pointer to the heliostat of this BCSHistory */
);
  
/* Queue of commands for BCS Processing */
CREATE TABLE BCSQueue(
  Instance_BCSQueue INTEGER PRIMARY KEY,                               /* Primary Key for the BCSQueue Table */
  Command_BCSQueue VARCHAR(128) DEFAULT '',                            /* The command in the queue */
  I$Heliostat_BCSQueue INTEGER DEFAULT 0,                              /* The heliostat for the command */
  I$BCS_BCSQueue INTEGER DEFAULT 0,                                    /* The BCS target for the command */
  EncoderAzCmd_BCSQueue INTEGER DEFAULT 0,                             /* The heliostat azimuthal encoder position (counts) */
  EncoderElCmd_BCSQueue INTEGER DEFAULT 0,                             /* The heliostat elevation encoder position (counts) */
  TimeTagHab_BCSQueue TIMESTAMPTZ DEFAULT now()                        /* The time tag when the image was taken */
);
  
/* Processes used to recalculate the Centroids for all BCS Images */
CREATE TABLE ImageProcess(
  Instance_ImageProcess INTEGER PRIMARY KEY,                           /* Primary Key for the ImageProcess Table */
  ID_ImageProcess VARCHAR(32) DEFAULT '',                              /* ID of the image reprocessing thread */
  I$BCS_ImageProcess INTEGER DEFAULT 0,                                /* The BCS camera of the image being processed */
  I$BCSHistory_ImageProcess INTEGER DEFAULT 0,                         /* The BCSHistory index of the image being processed */
  I$Heliostat_ImageProcess INTEGER DEFAULT 0,                          /* The heliostat index of the image being processed */
  TimeString_ImageProcess VARCHAR(32) DEFAULT '',                      /* Time Stamp for the image processing */
  TimeStringDone_ImageProcess VARCHAR(24) DEFAULT '',                  /* Time Stamp for the image processing completed */
  PathDone_ImageProcess VARCHAR(80) DEFAULT '',                        /* Image Path being processed */
  AngleOrientation_ImageProcess DOUBLE PRECISION DEFAULT 0.0,          /* The orientation of the image (degrees) */
  Area_ImageProcess DOUBLE PRECISION DEFAULT 0.0,                      /* The area of the image being processed (m^2) */
  XCentroid_ImageProcess DOUBLE PRECISION DEFAULT 0.0,                 /* The horizontal target coordinate of the image centroid (m) */
  YCentroid_ImageProcess DOUBLE PRECISION DEFAULT 0.0,                 /* The vertical target coordinate of the image centroid (m) */
  State_ImageProcess INTEGER DEFAULT 0,                                /* The state of the image processing task */
    Request_ImageProcess BOOLEAN DEFAULT false,                        /* Request Image Analysis to start */
    Completed_ImageProcess BOOLEAN DEFAULT false,                      /* Image Analysis is complete */
    Exit_ImageProcess BOOLEAN DEFAULT false                            /* Exit Flag */
);
  
/* Power Receiver */
CREATE TABLE Receiver(
  Instance_Receiver INTEGER PRIMARY KEY,                               /* Primary Key for the Receiver Table */
  Qtelemetry_Receiver DOUBLE PRECISION DEFAULT 0.0,                    /* Receiver power calculated from telemetry (increase in enthalpy of fluid) (MWt) */
  Qfiltered_Receiver DOUBLE PRECISION DEFAULT 0.0,                     /* Receiver power calculated from filtered telemetry (increase in enthalpy of fluid) (MWt) */
  ID_Receiver VARCHAR(32) DEFAULT '',                                  /* The name of a receiver */
  Diameter_Receiver DOUBLE PRECISION DEFAULT 0.0,                      /* Diameter (m) */
  DiameterBase_Receiver DOUBLE PRECISION DEFAULT 0.0,                  /* Base Diameter (m) */
  Height_Receiver DOUBLE PRECISION DEFAULT 0.0,                        /* Height of the active region of the receiver (m) */
  HeightBase_Receiver DOUBLE PRECISION DEFAULT 0.0,                    /* Base Height (m) */
  HeightTop_Receiver DOUBLE PRECISION DEFAULT 0.0,                     /* Top Height (m) */
  HeightTotal_Receiver DOUBLE PRECISION DEFAULT 0.0,                   /* Total Height of the Receiver */
  ElevationTotal_Receiver DOUBLE PRECISION DEFAULT 0.0,                /* Total Elevation of the receiver (m) */
  Elevation_Receiver DOUBLE PRECISION DEFAULT 0.0,                     /* Elevation of the base of the active region of the receiver (m) */
  Center_Receiver DOUBLE PRECISION DEFAULT 0.0,                        /* Elevation of the center of the active region of the receiver (m) */
  WidthPanel_Receiver DOUBLE PRECISION DEFAULT 0.0,                    /* The width of a panel including panel and tube gaps (m) */
  Panels_Receiver INTEGER DEFAULT 0,                                   /* The number of panels comprising the receiver */
  TubesPerPanel_Receiver INTEGER DEFAULT 0,                            /* The number of tubes per panel for the receiver */
  GapTube_Receiver DOUBLE PRECISION DEFAULT 0.0,                       /* The gap between tubes in the receiver (m) */
  GapPanel_Receiver DOUBLE PRECISION DEFAULT 0.0,                      /* The gap between panels in the receiver (m) */
  DiameterTube_Receiver DOUBLE PRECISION DEFAULT 0.0,                  /* The outer diameter of the Receiver Tubes (m) */
  ThicknessTubeWall_Receiver DOUBLE PRECISION DEFAULT 0.0,             /* The wall thickness of the receiver tubes (m) */
  ThicknessPaint_Receiver DOUBLE PRECISION DEFAULT 0.0,                /* The receiver tube paint thickness (m) */
  Reflectivity_Receiver DOUBLE PRECISION DEFAULT 0.0,                  /* The reflectivity of the receiver tubes (ratio) */
  Emissivity_Receiver DOUBLE PRECISION DEFAULT 0.0,                    /* The emissivity of the receiver tubes (ratio) */
  EmissivityHeatShield_Receiver DOUBLE PRECISION DEFAULT 0.0,          /* The emissivity of the heat shields (ratio) */
  ReflectivityHeatShield_Receiver DOUBLE PRECISION DEFAULT 0.0,        /* The reflectivity of the heat shields (ratio) */
  ElevationDeck_Receiver DOUBLE PRECISION DEFAULT 0.0,                 /* The elevation of the receiver deck (m) */
  RadiusDeck_Receiver DOUBLE PRECISION DEFAULT 0.0,                    /* The radius of the receiver deck (m) */
  ExtentDeck_Receiver DOUBLE PRECISION DEFAULT 0.0,                    /* The extent of the receiver deck out from the receiver (m) */
  HeightDeckFence_Receiver DOUBLE PRECISION DEFAULT 0.0,               /* The height of the fence around the receiver deck (m) */
  ThicknessOfDeckBase_Receiver DOUBLE PRECISION DEFAULT 0.0,           /* The thickness of the concrete and steel supporting base (m) */
  ElevationLowerHeader_Receiver DOUBLE PRECISION DEFAULT 0.0,          /* The elevation above ground of the lower header (m) */
  ElevationUpperHeader_Receiver DOUBLE PRECISION DEFAULT 0.0,          /* The elevation above ground of the upper header (m) */
  DiameterHeader_Receiver DOUBLE PRECISION DEFAULT 0.0,                /* The diameter of the tubing in the header (in) */
  LengthTube_Receiver DOUBLE PRECISION DEFAULT 0.0,                    /* The actual lenth of the tubing in the panel (ft) */
  NameFluid_Receiver VARCHAR(32) DEFAULT '',                           /* The name of the fluid in a receiver */
  WflowOperationMax_Receiver DOUBLE PRECISION DEFAULT 0.0,             /* The operational maximum flow rate for the receiver (lbm/s) */
  WflowOperationMin_Receiver DOUBLE PRECISION DEFAULT 0.0,             /* The operational minimum flow rate for the receiver (lbm/s) */
  WflowMax_Receiver DOUBLE PRECISION DEFAULT 0.0,                      /* The physical maximum flow rate limit for the receiver (lbm/s) */
  I$Flow_Receiver INTEGER DEFAULT 0,                                   /* Pointer to the total receiver flow */
  Drain$Valve_Receiver INTEGER DEFAULT 0,                              /* Pointer to the 1st drain valve of the receiver */
  STH$Level_Receiver INTEGER DEFAULT 0,                                /* Pointer to the short term hold level of the receiver */
  Panel$Level_Receiver INTEGER DEFAULT 0,                              /* Pointer to the 1st Panel Level of the receiver */
  ECV$Level_Receiver INTEGER DEFAULT 0,                                /* Pointer to the ECV Tank Level of the receiver */
  Out$Thermocouple_Receiver INTEGER DEFAULT 0,                         /* Pointer to thermocouple of the receiver outlet */
  Type_Receiver INTEGER DEFAULT 0,                                     /* The type of receiver */
    Fluid_Receiver BOOLEAN DEFAULT false,                              /* The receiver uses a fluid to capture heat. */
    Particle_Receiver BOOLEAN DEFAULT false,                           /* The receiver uses a particle stream to capture heat. */
  P$Field_Receiver INTEGER DEFAULT 0,                                  /* The field where the receiver is located */
  P$Panel_Receiver INTEGER DEFAULT 0,                                  /* Pointer to the starting panel of the receiver */
  P$Circuit_Receiver INTEGER DEFAULT 0,                                /* Pointer to the starting flow circuit of the receiver */
  SpeedWind_Receiver DOUBLE PRECISION DEFAULT 0.0,                     /* Receiver wind speed (m/s) */
  AngleWind_Receiver DOUBLE PRECISION DEFAULT 0.0,                     /* Direction of receiver windspeed, measured clockwise from north (degrees) */
  SourceData_Receiver INTEGER DEFAULT 0,                               /* The souce of data (Telemetered, Estimated or Manually Replaced) */
    SpeedWindTelemetry_Receiver BOOLEAN DEFAULT false,                 /* The telemetered value is used */
    SpeedWindEstimated_Receiver BOOLEAN DEFAULT false,                 /* The estimated value is used */
    SpeedWindManual_Receiver BOOLEAN DEFAULT false,                    /* The manually replaced value is used */
    AngleWindTelemetry_Receiver BOOLEAN DEFAULT false,                 /* The telemetered value is used */
    AngleWindEstimated_Receiver BOOLEAN DEFAULT false,                 /* The estimated value is used */
    AngleWindManual_Receiver BOOLEAN DEFAULT false,                    /* The manually replaced value is used */
  State_Receiver INTEGER DEFAULT 0,                                    /* The actual state of the receiver */
    Fill_Receiver BOOLEAN DEFAULT false,                               /* The Receiver is Filling */
    Operate_Receiver BOOLEAN DEFAULT false,                            /* The Receiver is Operating */
    Drain_Receiver BOOLEAN DEFAULT false,                              /* The Receiver is Draining */
    ShortTermHold_Receiver BOOLEAN DEFAULT false,                      /* The Receiver is at Short Term Hold */
    LongTermHold_Receiver BOOLEAN DEFAULT false,                       /* The Receiver is at Long Term Hold */
  Status_Receiver INTEGER DEFAULT 0,                                   /* The protection status of the receiver */
    NoWarningsOrTrips_Receiver BOOLEAN DEFAULT false,                  /* There are not active trips of warnings */
    AtEmergencyUnknown_Receiver BOOLEAN DEFAULT false,                 /* It cannot be determined if flux is on the receiver */
    WarnOnTempInnerWall_Receiver BOOLEAN DEFAULT false,                /* Maximum tube innerwall temperature is above the warning limit, but below the trip limit */
    TripOnTempInnerWall_Receiver BOOLEAN DEFAULT false,                /* Maximum tube tube innerwall temperature is above the trip limit */
    WarnOnTempIRCameras_Receiver BOOLEAN DEFAULT false,                /* Maximum IR temperature is above the warning limit, but below the trip limit */
    TripOnTempIRCameras_Receiver BOOLEAN DEFAULT false,                /* Maximum IR temperature is above the trip limit */
    WarnOnTempBackwalls_Receiver BOOLEAN DEFAULT false,                /* Maximum tube backwall temperature is above the warning limit, but below the trip limit */
    TripOnTempBackwalls_Receiver BOOLEAN DEFAULT false,                /* Maximum tube backwall temperature is above the trip limit */
  StatusIR_Receiver INTEGER DEFAULT 0,                                 /* IR Camera Receiver protection status */
    NoViolationIR_Receiver BOOLEAN DEFAULT false,                      /* There are no IR Camera active trips of warnings */
    ViolationIRWarn_Receiver BOOLEAN DEFAULT false,                    /* There are actove IR Camera temperature warnings */
    ViolationIRTrip_Receiver BOOLEAN DEFAULT false,                    /* There are actove IR Camera temperature trips */
  StatusIW_Receiver INTEGER DEFAULT 0,                                 /* Innerwall temperature Receiver protection status */
    NoViolationIW_Receiver BOOLEAN DEFAULT false,                      /* There are no tube innerwall active trips of warnings */
    ViolationIWWarn_Receiver BOOLEAN DEFAULT false,                    /* There are actove tube innerwall temperature warnings */
    ViolationIWTrip_Receiver BOOLEAN DEFAULT false,                    /* There are actove tube innerwall temperature trips */
  QIncidentReport_Receiver DOUBLE PRECISION DEFAULT 0.0,               /* The power incident on the receiver in this report (MW-Thermal) */
  QLossReflectionReport_Receiver DOUBLE PRECISION DEFAULT 0.0,         /* The power reflected off the receiver in this report (MW-Thermal) */
  QLossConductionReport_Receiver DOUBLE PRECISION DEFAULT 0.0,         /* The power conduction losses to air from the receiver in this report (MW-Thermal) */
  QLossRadiationReport_Receiver DOUBLE PRECISION DEFAULT 0.0,          /* The radiation losses from the receiver in this report (MW-Thermal) */
  QLossPipingReport_Receiver DOUBLE PRECISION DEFAULT 0.0,             /* The piping losses in the receiver in this report (MW-Thermal) */
  QLossMetalReport_Receiver DOUBLE PRECISION DEFAULT 0.0,              /* The power loss/gain from the metal capacitance of the receiver in this report (MW-Thermal) */
  QsaltReport_Receiver DOUBLE PRECISION DEFAULT 0.0,                   /* The power output of the receiver in this report (MW-Thermal) */
  WFlowReport_Receiver DOUBLE PRECISION DEFAULT 0.0,                   /* The output flow rate of the receiver in this report (lbms/second) */
  QIncidentEstimated_Receiver DOUBLE PRECISION DEFAULT 0.0,            /* The power incident on the receiver estimated by FluxCalc  (MW-Thermal) */
  WFlowEstimated_Receiver DOUBLE PRECISION DEFAULT 0.0,                /* The output flow rate of the receiver estimated by FluxCalc (lbms/second) */
  Qestimated_Receiver DOUBLE PRECISION DEFAULT 0.0,                    /* The power output of the receiver estimated by FluxCalc (MW-Thermal) */
  TempFOutLetEstimated_Receiver DOUBLE PRECISION DEFAULT 0.0,          /* The outlet temperature of the receiver estimated by FluxCalc (degF) */
  TempCOutLetEstimated_Receiver DOUBLE PRECISION DEFAULT 0.0,          /* The outlet temperature of the receiver estimated by FluxCalc (degC) */
  ReflectivityEstimated_Receiver DOUBLE PRECISION DEFAULT 0.0,         /* The mirror reflectivity for the receiver estimated by FluxCalc (fraction) */
  ExtraSpillageEstimated_Receiver DOUBLE PRECISION DEFAULT 0.0,        /* The excess spillage for energy balance estimated by FluxCalc (MW-Thermal) */
  BalanceFactorUsed_Receiver DOUBLE PRECISION DEFAULT 0.0              /* The Balance Factor for the Receiver used by FluxCalc (fraction */
);
  
/* Receiver Flow Circuit */
CREATE TABLE Circuit(
  Instance_Circuit INTEGER PRIMARY KEY,                                /* Primary Key for the Circuit Table */
  WflowClearSky_Circuit DOUBLE PRECISION DEFAULT 0.0,                  /* The clear sky flow of the circuit (lb/s) */
  TempFInletFiltered_Circuit DOUBLE PRECISION DEFAULT 0.0,             /* Inlet Filtered Temperature for each circuit (degF) */
  TempFOutletFiltered_Circuit DOUBLE PRECISION DEFAULT 0.0,            /* Outlet Filtered Temperature for each circuit (degF) */
  Qtelemetry_Circuit DOUBLE PRECISION DEFAULT 0.0,                     /* Circuit power calculated from telemetry (increase in enthalpy of fluid) (MWt) */
  Qfiltered_Circuit DOUBLE PRECISION DEFAULT 0.0,                      /* Circuit power calculated from filtered telemetry (increase in enthalpy of fluid) (MWt) */
  ID_Circuit VARCHAR(32) DEFAULT '',                                   /* The name of a receiver flow circuit */
  Inlet$Flow_Circuit INTEGER DEFAULT 0,                                /* Pointer to the inlet flow of the circuit */
  Outlet$Flow_Circuit INTEGER DEFAULT 0,                               /* Pointer to the outlet flow of the circuit */
  Inlet$Thermocouple_Circuit INTEGER DEFAULT 0,                        /* Pointer to the inlet flow of the circuit */
  Outlet$Thermocouple_Circuit INTEGER DEFAULT 0,                       /* Pointer to the outlet flow of the circuit */
  Inlet$Pressure_Circuit INTEGER DEFAULT 0,                            /* Pointer to the inlet pressure of the circuit */
  Outlet$Pressure_Circuit INTEGER DEFAULT 0,                           /* Pointer to the outlet pressure of the circuit */
  Start$Panel_Circuit INTEGER DEFAULT 0,                               /* Pointer to the starting panel of the circuit */
  Crossover$Panel_Circuit INTEGER DEFAULT 0,                           /* Pointer to the crossover start panel of the circuit */
  Continue$Panel_Circuit INTEGER DEFAULT 0,                            /* Pointer to the crossover continuation panel of the circuit */
  End$Panel_Circuit INTEGER DEFAULT 0,                                 /* Pointer to the outlet panel of the circuit */
  QsaltReport_Circuit DOUBLE PRECISION DEFAULT 0.0,                    /* The power output of the each receiver circuit in this report (MW-Thermal) */
  WFlowReport_Circuit DOUBLE PRECISION DEFAULT 0.0,                    /* The output flow rate of each receiver circuit in this report (lbms/second) */
  TempFinReport_Circuit DOUBLE PRECISION DEFAULT 0.0,                  /* The inlet temperature of each receiver circuit in this report (degF) */
  TempFoutReport_Circuit DOUBLE PRECISION DEFAULT 0.0,                 /* The outlet temperature of each receiver circuit in this report (degF) */
  WFlowEstimated_Circuit DOUBLE PRECISION DEFAULT 0.0,                 /* The output flow rate of each receiver circuit estimated by FluxCalc (lbms/second) */
  WFlowEstimatedVent_Circuit DOUBLE PRECISION DEFAULT 0.0,             /* The output flow rate up into the vent estimated by FluxCalc (lbms/second) */
  Qestimated_Circuit DOUBLE PRECISION DEFAULT 0.0,                     /* The power output of the each receiver circuit estimated by FluxCalc (MW-Thermal) */
  TempFInletEstimated_Circuit DOUBLE PRECISION DEFAULT 0.0,            /* The inlet temperature of each receiver circuit estimated by FluxCalc (degF) */
  TempCInletEstimated_Circuit DOUBLE PRECISION DEFAULT 0.0,            /* The inlet temperature of each receiver circuit estimated by FluxCalc (degC) */
  TempFOutLetEstimated_Circuit DOUBLE PRECISION DEFAULT 0.0,           /* The outlet temperature of each receiver circuit estimated by FluxCalc (degF) */
  TempCOutLetEstimated_Circuit DOUBLE PRECISION DEFAULT 0.0,           /* The outlet temperature of each receiver circuit estimated by FluxCalc (degC) */
  BalanceFactorEstimated_Circuit DOUBLE PRECISION DEFAULT 0.0          /* The Balance Factor calculated by FluxCalc for each circuit (fraction) */
);
  
/* Receiver Flow Circuit to Panel Mapping */
CREATE TABLE CktPnl(
  Instance_CktPnl INTEGER PRIMARY KEY,                                 /* Primary Key for the CktPnl Table */
  P$Circuit_CktPnl INTEGER DEFAULT 0,                                  /* Pointer to the parent circuit of a circuit panel */
  I$Panel_CktPnl INTEGER DEFAULT 0                                     /* Pointer to the panel of a circuit panel */
);
  
/* Receiver Panels */
CREATE TABLE Panel(
  Instance_Panel INTEGER PRIMARY KEY,                                  /* Primary Key for the Panel Table */
  NumHeliostatsTrack_Panel INTEGER DEFAULT 0,                          /* Number of heliostats in the tracking mode */
  NumHeliostatsTracking_Panel INTEGER DEFAULT 0,                       /* Number of heliostats tracking */
  NumHeliostatsStow_Panel INTEGER DEFAULT 0,                           /* Number of heliostats in emergency stow mode */
  NumHeliostatsStowed_Panel INTEGER DEFAULT 0,                         /* Number of heliostats at emergency stow */
  NumHeliostatsDefocus_Panel INTEGER DEFAULT 0,                        /* Number of heliostats in emergency defocus mode */
  NumHeliostatsDefocused_Panel INTEGER DEFAULT 0,                      /* Number of heliostats at emergency defocus */
  NumHeliostatsSleep_Panel INTEGER DEFAULT 0,                          /* Number of heliostats in sleep mode */
  NumHeliostatsSleeping_Panel INTEGER DEFAULT 0,                       /* Number of heliostats asleep */
  NumHeliostatsWash_Panel INTEGER DEFAULT 0,                           /* Number of heliostats in rainwash mode */
  NumHeliostatsAtWash_Panel INTEGER DEFAULT 0,                         /* Number of heliostats at wash position */
  NumHeliostatsAtRainWash_Panel INTEGER DEFAULT 0,                     /* Number of heliostats at rain wash position */
  NumHeliostatsRainWash_Panel INTEGER DEFAULT 0,                       /* Number of heliostats in wash mode */
  NumHeliostatsAtDefrost_Panel INTEGER DEFAULT 0,                      /* Number of heliostats at defrost position */
  NumHeliostatsDefrost_Panel INTEGER DEFAULT 0,                        /* Number of heliostats in defrost mode */
  NumHeliostatsManual_Panel INTEGER DEFAULT 0,                         /* Number of heliostats in manual position mode */
  NumHeliostatsAtManual_Panel INTEGER DEFAULT 0,                       /* Number of heliostats at the manually position */
  NumHeliostatsStop_Panel INTEGER DEFAULT 0,                           /* Number of heliostats in emergency stop mode */
  NumHeliostatsStopped_Panel INTEGER DEFAULT 0,                        /* Number of heliostats succefully stopped for emergency purposes */
  NumHeliostatsStandby_Panel INTEGER DEFAULT 0,                        /* Number of heliostats in standby mode */
  NumHeliostatsAtStandby_Panel INTEGER DEFAULT 0,                      /* Number of heliostats at the standby position */
  NumHeliostatsBCS_Panel INTEGER DEFAULT 0,                            /* Number of heliostats being processed by the BCS */
  NumHeliostatsAtBCS_Panel INTEGER DEFAULT 0,                          /* Number of heliostats being imaged by the BCS */
  NumHeliostatsAvailable_Panel INTEGER DEFAULT 0,                      /* Number of heliostats able to be controlled */
  NumHeliostatsUnavailable_Panel INTEGER DEFAULT 0,                    /* Number of heliostats unable to be controlled */
  NumHeliostatsTransition_Panel INTEGER DEFAULT 0,                     /* Number of heliostats in transition */
  NumHeliostatsOffline_Panel INTEGER DEFAULT 0,                        /* Number of heliostats Offline */
  NumHeliostatsStuckDown_Panel INTEGER DEFAULT 0,                      /* Number of heliostats that can't be controlled by the HFCS */
  NumHeliostats_Panel INTEGER DEFAULT 0,                               /* Number of Heliostats assigned to a panel */
  TemperatureUpperAverage_Panel DOUBLE PRECISION DEFAULT 0.0,          /* Temperature average of the upper thermocouples in the panel (System Units) */
  TemperatureLowerAverage_Panel DOUBLE PRECISION DEFAULT 0.0,          /* Temperature average of the lower thermocouples in the panel (System Units) */
  TemperatureMiddleAverage_Panel DOUBLE PRECISION DEFAULT 0.0,         /* Temperature average of the middle thermocouples in the panel (System Units) */
  TemperatureInletAverage_Panel DOUBLE PRECISION DEFAULT 0.0,          /* Temperature average of the inlet thermocouples in the panel (System Units) */
  TemperatureOutletAverage_Panel DOUBLE PRECISION DEFAULT 0.0,         /* Temperature average of the outlet thermocouples in the panel (System Units) */
  TemperatureAverage_Panel DOUBLE PRECISION DEFAULT 0.0,               /* Temperature average of all the thermocouples in the panel (System Units) */
  TemperatureIRMax_Panel DOUBLE PRECISION DEFAULT 0.0,                 /* Maximum IR temperature on the panel surface (System Units) */
  TemperatureIRMin_Panel DOUBLE PRECISION DEFAULT 0.0,                 /* Minimum IR temperature on the panel surface (System Units) */
  TempIRMax$RowPnl_Panel INTEGER DEFAULT 0,                            /* Location on the Panel of the maximum IR temperature */
  TempFUpperHeatShieldIRMax_Panel DOUBLE PRECISION DEFAULT 0.0,        /* Maximum IR temperature on the panel upper heat shields (System Units) */
  TempFLowerHeatShieldIRMax_Panel DOUBLE PRECISION DEFAULT 0.0,        /* Maximum IR temperature on the panel lower heat shields (System Units) */
  TempIRMax$RowHSUpper_Panel INTEGER DEFAULT 0,                        /* Location on the panel upper heatshields of the maximum IR temperature */
  TempIRMax$RowHSLower_Panel INTEGER DEFAULT 0,                        /* Location on the panel lower heatshields of the maximum IR temperature */
  TemperatureUpperMax_Panel DOUBLE PRECISION DEFAULT 0.0,              /* Maximum temperature of the upper thermocouples in the panel (System Units) */
  TemperatureUpperMin_Panel DOUBLE PRECISION DEFAULT 0.0,              /* Minimum temperature of the upper thermocouples in the panel (System Units) */
  TemperatureLowerMax_Panel DOUBLE PRECISION DEFAULT 0.0,              /* Maximum temperature of the lower thermocouples in the panel (System Units) */
  TemperatureLowerMin_Panel DOUBLE PRECISION DEFAULT 0.0,              /* Minimum temperature of the lower thermocouples in the panel (System Units) */
  TemperatureMiddleMax_Panel DOUBLE PRECISION DEFAULT 0.0,             /* Maximum temperature of the middle thermocouples in the panel (System Units) */
  TemperatureMiddleMin_Panel DOUBLE PRECISION DEFAULT 0.0,             /* Minimum temperature of the middle thermocouples in the panel (System Units) */
  dTemperatureWithNext_Panel DOUBLE PRECISION DEFAULT 0.0,             /* Temperature Delta From Panel Outlet to the Next Downstream Panel Inlet (System Units) */
  dTemp_Panel DOUBLE PRECISION DEFAULT 0.0,                            /* The temperature delta across the panel (in system units) */
  QtelemetryUpper_Panel DOUBLE PRECISION DEFAULT 0.0,                  /* Power accumlation from inlet at the upper portion of the panel (increase in enthalpy of fluid) (MWt) */
  QtelemetryLower_Panel DOUBLE PRECISION DEFAULT 0.0,                  /* Power accumlation from inlet at the lower portion of the panel (increase in enthalpy of fluid) (MWt) */
  ID_Panel VARCHAR(32) DEFAULT '',                                     /* The name of a receiver panel */
  Emissivity_Panel DOUBLE PRECISION DEFAULT 0.0,                       /* The emissivity of the receiver panel (ratio) */
  EmissivityHeatShield_Panel DOUBLE PRECISION DEFAULT 0.0,             /* The emissivity of the heat shields per panel (ratio) */
  Reflectivity_Panel DOUBLE PRECISION DEFAULT 0.0,                     /* The reflectivity of the receiver panel (ratio) */
  AngleAzimuthal_Panel DOUBLE PRECISION DEFAULT 0.0,                   /* Azimuthal angle of the panel (clockwise from north) (degrees) */
  TimeInstalled_Panel TIMESTAMPTZ DEFAULT now(),                       /* The time the panel was installed */
  Inlet$Thermocouple_Panel INTEGER DEFAULT 0,                          /* Pointer to the inlet temperature measurements for this panel */
  Outlet$Thermocouple_Panel INTEGER DEFAULT 0,                         /* Pointer to the outlet temperature measurements for this panel */
  Mid$Thermocouple_Panel INTEGER DEFAULT 0,                            /* Pointer to the midpoint temperature measurements for this panel */
  I$Wedge_Panel INTEGER DEFAULT 0,                                     /* The heliostat field wedge focused on this panel */
  I$Level_Panel INTEGER DEFAULT 0,                                     /* The level associated with this panel */
  I$Circuit_Panel INTEGER DEFAULT 0,                                   /* The flow circuit of this panel */
  P$Receiver_Panel INTEGER DEFAULT 0,                                  /* The receiver the panel is part of */
  P$PnlGroup_Panel INTEGER DEFAULT 0,                                  /* Hierarchical pointer to the groups heliostats that will thaw this panel */
  PctPowerPreheat_Panel DOUBLE PRECISION DEFAULT 0.0,                  /* The operator entered percent of heliostats that the operator has selected (percent) */
  PctPowerReceiverThaw_Panel DOUBLE PRECISION DEFAULT 0.0,             /* The operator entered percent of heliostats that the operator has selected (percent) */
  PctStrainLimitAdjusted_Panel DOUBLE PRECISION DEFAULT 0.0,           /* The percent by which to adjust the displayed strain limits (percent) */
  TCContactLoosenessUpper_Panel DOUBLE PRECISION DEFAULT 0.0,          /* The looseness of the panel upper thermocouple contacts with the receiver tube [0,infinity] */
  TCContactLoosenessMiddle_Panel DOUBLE PRECISION DEFAULT 0.0,         /* The looseness of the panel middle thermocouple contacts with the receiver tube [0,infinity] */
  TCContactLoosenessLower_Panel DOUBLE PRECISION DEFAULT 0.0,          /* The looseness of the panel lower thermocouple contacts with the receiver tube [0,infinity] */
  State_Panel INTEGER DEFAULT 0,                                       /* The UI Operator can issue the following preheat/postheat booster commands for the panel */
    BoostersUpper_Panel BOOLEAN DEFAULT false,                         /* Turn the panel upper preheat/postheat boosters on/off */
    BoostersMiddle_Panel BOOLEAN DEFAULT false,                        /* Turn the panel middle preheat/postheat boosters on/off */
    BoostersLower_Panel BOOLEAN DEFAULT false,                         /* Turn the panel lower preheat/postheat boosters on/off */
  TempWarnLimitLower_Panel DOUBLE PRECISION DEFAULT 0.0,               /* The temperature warning limit for the lower thermcouples in this panel (System Units) */
  TempWarnLimitMid_Panel DOUBLE PRECISION DEFAULT 0.0,                 /* The temperature warning limit for the middle thermcouples in this panel (System Units) */
  TempWarnLimitUpper_Panel DOUBLE PRECISION DEFAULT 0.0,               /* The temperature warning limit for the upper thermcouples in this panel (System Units) */
  TempTripLimitLower_Panel DOUBLE PRECISION DEFAULT 0.0,               /* The temperature trip limit for the lower thermcouples in this panel (System Units) */
  TempTripLimitMid_Panel DOUBLE PRECISION DEFAULT 0.0,                 /* The temperature trip limit for the middle thermcouples in this panel (System Units) */
  TempTripLimitUpper_Panel DOUBLE PRECISION DEFAULT 0.0,               /* The temperature trip limit for the upper thermcouples in this panel (System Units) */
  TempFWarnLimitLower_Panel DOUBLE PRECISION DEFAULT 0.0,              /* The temperature warning limit for the lower thermcouples in this panel ((DegF) */
  TempFWarnLimitMid_Panel DOUBLE PRECISION DEFAULT 0.0,                /* The temperature warning limit for the middle thermcouples in this panel ((DegF) */
  TempFWarnLimitUpper_Panel DOUBLE PRECISION DEFAULT 0.0,              /* The temperature warning limit for the upper thermcouples in this panel ((DegF) */
  TempFTripLimitLower_Panel DOUBLE PRECISION DEFAULT 0.0,              /* The temperature trip limit for the lower thermcouples in this panel ((DegF) */
  TempFTripLimitMid_Panel DOUBLE PRECISION DEFAULT 0.0,                /* The temperature trip limit for the middle thermcouples in this panel ((DegF) */
  TempFTripLimitUpper_Panel DOUBLE PRECISION DEFAULT 0.0,              /* The temperature trip limit for the upper thermcouples in this panel ((DegF) */
  TempCWarnLimitLower_Panel DOUBLE PRECISION DEFAULT 0.0,              /* The temperature warning limit for the lower thermcouples in this panel ((DegC) */
  TempCWarnLimitMid_Panel DOUBLE PRECISION DEFAULT 0.0,                /* The temperature warning limit for the middle thermcouples in this panel ((DegC) */
  TempCWarnLimitUpper_Panel DOUBLE PRECISION DEFAULT 0.0,              /* The temperature warning limit for the upper thermcouples in this panel ((DegC) */
  TempCTripLimitLower_Panel DOUBLE PRECISION DEFAULT 0.0,              /* The temperature trip limit for the lower thermcouples in this panel ((DegC) */
  TempCTripLimitMid_Panel DOUBLE PRECISION DEFAULT 0.0,                /* The temperature trip limit for the middle thermcouples in this panel ((DegC) */
  TempCTripLimitUpper_Panel DOUBLE PRECISION DEFAULT 0.0,              /* The temperature trip limit for the upper thermcouples in this panel ((DegC) */
  QincidentReport_Panel DOUBLE PRECISION DEFAULT 0.0,                  /* The power incident on each receiver panel in this report (MW-Thermal) */
  dQsaltReport_Panel DOUBLE PRECISION DEFAULT 0.0,                     /* The power gain in each receiver panel in this report (MW-Thermal) */
  TempFinReport_Panel DOUBLE PRECISION DEFAULT 0.0,                    /* The inlet temperature of each receiver panel in this report (degF) */
  TempFoutReport_Panel DOUBLE PRECISION DEFAULT 0.0,                   /* The outlet temperature of each receiver panel in this report (degF) */
  dTempFReport_Panel DOUBLE PRECISION DEFAULT 0.0,                     /* The temperature gain each receiver panel in this report (degF) */
  StrainMaxReport_Panel DOUBLE PRECISION DEFAULT 0.0,                  /* The maximum tube strain in each receiver panel in this report (percent) */
  TempFinnerWallMaxReport_Panel DOUBLE PRECISION DEFAULT 0.0,          /* The maximum innerwall temperature in each panel in this report (degF) */
  TempFouterWallMaxReport_Panel DOUBLE PRECISION DEFAULT 0.0,          /* The maximum outerwall temperature in each panel in this report (degF) */
  TempFavgWallMaxReport_Panel DOUBLE PRECISION DEFAULT 0.0,            /* The maximum average wall temperature in each panel in this report (degF) */
  FluxHeatShieldMaxReport_Panel DOUBLE PRECISION DEFAULT 0.0,          /* The maximum heat shield flux on each panel in this report (kw/m^2) */
  EfficiencyActual_Panel DOUBLE PRECISION DEFAULT 0.0,                 /* The ratio of actual energy to ray-traced energy for each panel (fraction) */
  QIncidentEstimated_Panel DOUBLE PRECISION DEFAULT 0.0,               /* The power incident on each panel estimated by FluxCalc  (MW-Thermal) */
  StrainMax$Rowpnl_Panel INTEGER DEFAULT 0,                            /* The location on the panel with the maximum strain */
  TempIWMax$Rowpnl_Panel INTEGER DEFAULT 0,                            /* The location on the panel with the maximum innerwall temperature */
  StatusViolation_Panel INTEGER DEFAULT 0,                             /* Trip and Warning violations status for the receiver panel */
    NoViolation_Panel BOOLEAN DEFAULT false,                           /* The Inner wall temperature is below the warning limit */
    WarningViolation_Panel BOOLEAN DEFAULT false,                      /* The inner wall temperature is below the trip limit but above the warning limit */
    TripViolation_Panel BOOLEAN DEFAULT false                          /* The inner wall temperature is above the trip limit */
);
  
/* Groups of Heliostats to thaw Receiver Panels */
CREATE TABLE PnlGroup(
  Instance_PnlGroup INTEGER PRIMARY KEY,                               /* Primary Key for the PnlGroup Table */
  ID_PnlGroup VARCHAR(32) DEFAULT '',                                  /* The name of a receiver panel thawing group. */
  P$PnlThaw_PnlGroup INTEGER DEFAULT 0                                 /* Hierarchical pointer to the heliostats in a panel thaw group */
);
  
/* Set of Heliostats to thaw Receiver Panels */
CREATE TABLE PnlThaw(
  Instance_PnlThaw INTEGER PRIMARY KEY,                                /* Primary Key for the PnlThaw Table */
  I$Heliostat_PnlThaw INTEGER DEFAULT 0,                               /* Heliostat that will help thaw the panel */
  P$PnlGroup_PnlThaw INTEGER DEFAULT 0                                 /* The thaw group this is assigned to */
);
  
/* Columns for Receiver Flux Calculation and IR temperatures */
CREATE TABLE ColPnl(
  Instance_ColPnl INTEGER PRIMARY KEY,                                 /* Primary Key for the ColPnl Table */
  ID_ColPnl VARCHAR(16) DEFAULT ''                                     /* ID of each panel column */
);
  
/* Rows for Receiver Flux Calculation and IR Camera temperatures */
CREATE TABLE RowPnl(
  Instance_RowPnl INTEGER PRIMARY KEY,                                 /* Primary Key for the RowPnl Table */
  ID_RowPnl VARCHAR(16) DEFAULT '',                                    /* ID of the receiver panel row elements in each column */
  dZCenter_RowPnl DOUBLE PRECISION DEFAULT 0.0,                        /* This distance from the receiver center of each panel row element */
  Flux_RowPnl DOUBLE PRECISION DEFAULT 0.0,                            /* The flux on each panel row element estimated by FluxCalc (kw/m^2) */
  Strain_RowPnl DOUBLE PRECISION DEFAULT 0.0,                          /* The strain on each panel row element estimated by FluxCalc (percent) */
  TempFsurf_RowPnl DOUBLE PRECISION DEFAULT 0.0,                       /* The tube surface temperature of each panel row element estimated by FluxCalc (degF) */
  TempFouterwall_RowPnl DOUBLE PRECISION DEFAULT 0.0,                  /* The tube outerwall temperature of each panel row element estimated by FluxCalc (degF) */
  TempFinnerwall_RowPnl DOUBLE PRECISION DEFAULT 0.0,                  /* The tube innerwall temperature of each panel row element estimated by FluxCalc (degF) */
  TempFIRCamera_RowPnl DOUBLE PRECISION DEFAULT 0.0,                   /* The IR camera temperature of each panel row element (degF) */
  TempFIRCalculated_RowPnl DOUBLE PRECISION DEFAULT 0.0,               /* The tube paint temperature of each panel row element calculated by FluxCalc(degF) */
  TempFbackwall_RowPnl DOUBLE PRECISION DEFAULT 0.0,                   /* The tube backwall temperature of each panel row element estimated by FluxCalc (degF) */
  TempFbulk_RowPnl DOUBLE PRECISION DEFAULT 0.0,                       /* The tube bulk fluid temperature of each panel row element estimated by FluxCalc (degF) */
  IntensityValue_RowPnl INTEGER DEFAULT 0,                             /* Raw IR Camera intensity of each panel row element estimated by FluxCalc  (future) */
  NumHeliostatsFocused_RowPnl INTEGER DEFAULT 0,                       /* The number of heliostats focused on each panel row element */
  StatusIRData_RowPnl INTEGER DEFAULT 0,                               /* The data quality of the IR data for each panel row element */
    DataIsCurrent_RowPnl BOOLEAN DEFAULT false,                        /* The data is being updated */
    DataIsStale_RowPnl BOOLEAN DEFAULT false,                          /* The data is stale */
  Status_RowPnl INTEGER DEFAULT 0,                                     /* The status of this location on the receiver with respect to amining */
    TargetedByHeliostat_RowPnl BOOLEAN DEFAULT false,                  /* There is at least on heliostat aimed at this location on the receiver */
  Aim01$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim02$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim03$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim04$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim05$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim06$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim07$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim08$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim09$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim10$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim11$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim12$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim13$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim14$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim15$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim16$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim17$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim18$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim19$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim20$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim21$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim22$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim23$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim24$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim25$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim26$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim27$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim28$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim29$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim30$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim31$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim32$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim33$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim34$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim35$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim36$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim37$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim38$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim39$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim40$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim41$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim42$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim43$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim44$Heliostat_RowPnl INTEGER DEFAULT 0,                            /* The index of one of the heliostats aimed at this location on the receiver */
  Aim45$Heliostat_RowPnl INTEGER DEFAULT 0                             /* The index of one of the heliostats aimed at this location on the receiver */
);
  
/* Rows for Upper Heat Shield Flux Calculation and IR Camera temperatures */
CREATE TABLE RowHSUpper(
  Instance_RowHSUpper INTEGER PRIMARY KEY,                             /* Primary Key for the RowHSUpper Table */
  ID_RowHSUpper VARCHAR(16) DEFAULT '',                                /* ID of the upper heatshield panel row elements in each column */
  TempFsurf_RowHSUpper DOUBLE PRECISION DEFAULT 0.0,                   /* The surface temperature of each upper heatshield row element estimated by FluxCalc (degF) */
  TempFIRCamera_RowHSUpper DOUBLE PRECISION DEFAULT 0.0,               /* The IR Camera temperature of each upper heatshield row element (degF) */
  Flux_RowHSUpper DOUBLE PRECISION DEFAULT 0.0,                        /* The flux on each upper heatshield row element estimated by FluxCalc (kw/m^2) */
  IntensityValue_RowHSUpper INTEGER DEFAULT 0,                         /* Raw IR Camera intensity of each upper heatshield row element estimated by FluxCalc  (future) */
  StatusIRData_RowHSUpper INTEGER DEFAULT 0,                           /* The data quality of the IR data for each upper heatshield row element */
    DataIsCurrent_RowHSUpper BOOLEAN DEFAULT false,                    /* The data is being updated */
    DataIsStale_RowHSUpper BOOLEAN DEFAULT false                       /* The data is stale */
);
  
/* Rows for Lower Heat Shield Flux Calculation and IR Camera temperatures */
CREATE TABLE RowHSLower(
  Instance_RowHSLower INTEGER PRIMARY KEY,                             /* Primary Key for the RowHSLower Table */
  ID_RowHSLower VARCHAR(16) DEFAULT '',                                /* ID of the lower heatshield panel row elements in each column */
  TempFsurf_RowHSLower DOUBLE PRECISION DEFAULT 0.0,                   /* The surface temperature of each lower heatshield row element estimated by FluxCalc (degF) */
  TempFIRCamera_RowHSLower DOUBLE PRECISION DEFAULT 0.0,               /* The IR Camera temperature of each lower heatshield row element (degF) */
  Flux_RowHSLower DOUBLE PRECISION DEFAULT 0.0,                        /* The flux on each lower heatshield row element estimated by FluxCalc (kw/m^2) */
  IntensityValue_RowHSLower INTEGER DEFAULT 0,                         /* Raw IR Camera intensity of each lower heatshield row element estimated by FluxCalc  (future) */
  StatusIRData_RowHSLower INTEGER DEFAULT 0,                           /* The data quality of the IR data for each lower heatshield row element */
    DataIsCurrent_RowHSLower BOOLEAN DEFAULT false,                    /* The data is being updated */
    DataIsStale_RowHSLower BOOLEAN DEFAULT false                       /* The data is stale */
);
  
/* Power Tower */
CREATE TABLE Tower(
  Instance_Tower INTEGER PRIMARY KEY,                                  /* Primary Key for the Tower Table */
  ID_Tower VARCHAR(32) DEFAULT '',                                     /* The name of a tower */
  Diameter_Tower DOUBLE PRECISION DEFAULT 0.0,                         /* Diameter (m) */
  DiameterBase_Tower DOUBLE PRECISION DEFAULT 0.0,                     /* Base Diameter (m) */
  Height_Tower DOUBLE PRECISION DEFAULT 0.0,                           /* Height (m) */
  ElevationUpperBypass_Tower DOUBLE PRECISION DEFAULT 0.0              /* Elevation of the upper bypass line (m) */
);
  
/* Portion of field illuminating one receiver panel */
CREATE TABLE Wedge(
  Instance_Wedge INTEGER PRIMARY KEY,                                  /* Primary Key for the Wedge Table */
  PercentTracking_Wedge DOUBLE PRECISION DEFAULT 0.0,                  /* The percent of heliostats tracking in a field wedge */
  ID_Wedge VARCHAR(32) DEFAULT '',                                     /* The name of the field wedge */
  I$Panel_Wedge INTEGER DEFAULT 0,                                     /* The radiator panel associated with this field wedge */
  P$Section_Wedge INTEGER DEFAULT 0,                                   /* The first section in the wedge */
  P$Hstat_Wedge INTEGER DEFAULT 0,                                     /* The heliostat locations for this wedge */
  Adjust_Wedge DOUBLE PRECISION DEFAULT 0.0,                           /* The Gain Adjustment for heliostat aimpoints in the wedge */
  Retain_Wedge DOUBLE PRECISION DEFAULT 0.0,                           /* The fraction of heliostats to retain in the wedge (fraction) */
  Available_Wedge DOUBLE PRECISION DEFAULT 0.0,                        /* The fraction of heliostats available heliostat in this wedge (fraction) */
  dZMinDither_Wedge DOUBLE PRECISION DEFAULT 0.0,                      /* Field aimpoint minimum Z dither preheat motion  for this wedge (m) */
  dZMaxDither_Wedge DOUBLE PRECISION DEFAULT 0.0,                      /* Field aimpoint maximum Z dither preheat motion  for this wedge (m) */
  StatusSet_Wedge INTEGER DEFAULT 0,                                   /* The UI operator can command all selected heliostats in a wedge. */
    SelectWash_Wedge BOOLEAN DEFAULT false,                            /* Select that selected heliostats within this wedge be put into wash position. */
    SelectWashCancel_Wedge BOOLEAN DEFAULT false,                      /* Return selected heliostats in wash position in this wedge to the field control */
    SelectWashComplete_Wedge BOOLEAN DEFAULT false,                    /* Records selected heliostats wash complete */
    SelectRainPosition_Wedge BOOLEAN DEFAULT false,                    /* Moves selected heliostats within this wedge into rain wash position. */
    SelectDefrost_Wedge BOOLEAN DEFAULT false,                         /* Set selected heliostats within this wedge to Defrost. */
    SelectStandby_Wedge BOOLEAN DEFAULT false,                         /* Moves selected heliostats within this wedge into standby position. */
    SelectStow_Wedge BOOLEAN DEFAULT false,                            /* Moves selected heliostats within this wedge to wind stow position. */
    SelectSleep_Wedge BOOLEAN DEFAULT false,                           /* Moves selected heliostats within this wedge to night stow position. */
    SelectOperator_Wedge BOOLEAN DEFAULT false,                        /* Puts selected heliostats within this wedge into operator control mode. */
    SelectAutomatic_Wedge BOOLEAN DEFAULT false,                       /* Puts selected heliostats within this wedge into automatic control mode. */
  EndOperation$Hstat_Wedge INTEGER DEFAULT 0,                          /* The starting heliostat location that the operator has selected for this wedge (for common operations) */
  StartOperation$Hstat_Wedge INTEGER DEFAULT 0,                        /* The ending heliostat location that the operator has selected for this wedge (for common operations) */
  AdjustReport_Wedge DOUBLE PRECISION DEFAULT 0.0,                     /* The Gain adjustment to Zaim for a wedge for maximum power for this report (multiplier) */
  RetainReport_Wedge DOUBLE PRECISION DEFAULT 0.0,                     /* The heliostats retained in a wedge for maximum power for this report (fraction) */
  dZMinDitherReport_Wedge DOUBLE PRECISION DEFAULT 0.0,                /* The minimum dZ adjustment during preheat for each wedge in this report (m) */
  dZMaxDitherReport_Wedge DOUBLE PRECISION DEFAULT 0.0                 /* The maximum dZ adjustment during preheat for each wedge in this report (m) */
);
  
/* Radial divisions of a Wedge */
CREATE TABLE Section(
  Instance_Section INTEGER PRIMARY KEY,                                /* Primary Key for the Section Table */
  ID_Section VARCHAR(32) DEFAULT '',                                   /* The name of the wedge Radial section */
  P$Wedge_Section INTEGER DEFAULT 0,                                   /* The wedge this section is part of */
  P$Hstat_Section INTEGER DEFAULT 0,                                   /* The first heliostat location in this section */
  State_Section INTEGER DEFAULT 0,                                     /* The status of a wedge section */
    SelectionActive_Section BOOLEAN DEFAULT false,                     /* There are heliostat locations selected in this section. */
    Attention_Section BOOLEAN DEFAULT false                            /* This is no longer used */
);
  
/* Heliostats in a section */
CREATE TABLE Hstat(
  Instance_Hstat INTEGER PRIMARY KEY,                                  /* Primary Key for the Hstat Table */
  I$Heliostat_Hstat INTEGER DEFAULT 0,                                 /* The heliostat associated with this heliostat location */
  P$Wedge_Hstat INTEGER DEFAULT 0                                      /* The wedge this heliostat location is part of. */
);
  
/* Field Rings */
CREATE TABLE Ring(
  Instance_Ring INTEGER PRIMARY KEY,                                   /* Primary Key for the Ring Table */
  ID_Ring VARCHAR(32) DEFAULT '',                                      /* The name of a ring */
  Type_Ring INTEGER DEFAULT 0,                                         /* The orientation of the truck to wash heliostats in this ring */
    WashAzimuthalFacingTower_Ring BOOLEAN DEFAULT false,               /* Wash with the mirror facing the tower */
    WashAzimuthalBackToTower_Ring BOOLEAN DEFAULT false,               /* Wash with the mirror back facing the tower */
    WashAzimuthalSpecified_Ring BOOLEAN DEFAULT false,                 /* Wash with the azimuthal angle set in the AngleWashAzimuthal_Ring */
  AngleWashAzimuthal_Ring DOUBLE PRECISION DEFAULT 0.0,                /* The azimuthal position for washing when the WashAzimuthalSpecified Flag is Set (degrees) */
  Radius_Ring DOUBLE PRECISION DEFAULT 0.0,                            /* The minimum distance from the tower to the ring (m) */
  PageInDocument_Ring INTEGER DEFAULT 0,                               /* The page in the construction document for this ring. */
  P$Field_Ring INTEGER DEFAULT 0,                                      /* The field in which the ring is situated */
  P$Zone_Ring INTEGER DEFAULT 0,                                       /* The starting zone of the ring */
  P$Heliostat_Ring INTEGER DEFAULT 0,                                  /* The starting heliostat of a ring */
  StatusSet_Ring INTEGER DEFAULT 0,                                    /* The UI operator can command all selected heliostats in a ring. */
    SelectWash_Ring BOOLEAN DEFAULT false,                             /* Select that selected heliostats within this ring be put into wash position. */
    SelectWashCancel_Ring BOOLEAN DEFAULT false,                       /* Return selected heliostats in wash position in this ring back to field control */
    SelectWashComplete_Ring BOOLEAN DEFAULT false,                     /* Records selected heliostats wash complete */
    SelectRainPosition_Ring BOOLEAN DEFAULT false,                     /* Moves selected heliostats within this ring into rain wash position. */
    SelectDefrost_Ring BOOLEAN DEFAULT false,                          /* Set selected heliostats within this ring to Defrost. */
    SelectStandby_Ring BOOLEAN DEFAULT false,                          /* Moves selected heliostats within this ring into standby position. */
    SelectStow_Ring BOOLEAN DEFAULT false,                             /* Moves selected heliostats within this ring to wind stow position. */
    SelectSleep_Ring BOOLEAN DEFAULT false,                            /* Moves selected heliostats within this ring to night stow position. */
    SelectOperator_Ring BOOLEAN DEFAULT false,                         /* Puts selected heliostats within this ring into operator control mode. */
    SelectAutomatic_Ring BOOLEAN DEFAULT false,                        /* Puts selected heliostats within this ring into automatic command mode. */
  dZTarget_Ring DOUBLE PRECISION DEFAULT 0.0,                          /* UI Operator Target adjustment in  the Vertical Direction for heliostats in this Ring (m) */
  State_Ring INTEGER DEFAULT 0,                                        /* The status of the ring. */
    WashInProgress_Ring BOOLEAN DEFAULT false,                         /* There are heliostats being washed in this ring */
  StartWash$Heliostat_Ring INTEGER DEFAULT 0,                          /* The starting heliostat that the operator has selected for this ring (for common operations) */
  EndWash$Heliostat_Ring INTEGER DEFAULT 0                             /* The ending heliostat that the operator has selected for this ring (for common operations) */
);
  
/* Ring Zones */
CREATE TABLE Zone(
  Instance_Zone INTEGER PRIMARY KEY,                                   /* Primary Key for the Zone Table */
  ID_Zone VARCHAR(32) DEFAULT '',                                      /* The name of a zone */
  FocalLengthRadial_Zone DOUBLE PRECISION DEFAULT 0.0,                 /* Radial Focal Length in units of tower Height for each heliostat in each zone */
  XFocalLength_Zone DOUBLE PRECISION DEFAULT 0.0,                      /* User defined focal length in the X-axis for the Radial zone */
  YFocalLength_Zone DOUBLE PRECISION DEFAULT 0.0,                      /* User defined focal length in the Y-axis for the Radial zone */
  AngleAzimuthal_Zone DOUBLE PRECISION DEFAULT 0.0,                    /* Field Azimuthal Angle Determined from Clockwise Rotation from North (degrees) */
  StatusSet_Zone INTEGER DEFAULT 0,                                    /* The UI Operator can issue commands to selected heliostats in the zone */
    RequestWash_Zone BOOLEAN DEFAULT false,                            /* Request that all helisotats within this zone be put into wash position. */
    CancelWash_Zone BOOLEAN DEFAULT false,                             /* Return all heliostats in wash position back to system control */
  P$Ring_Zone INTEGER DEFAULT 0,                                       /* The ring this zone belongs to */
  P$Heliostat_Zone INTEGER DEFAULT 0,                                  /* The starting heliostat in this zone */
  dXTarget_Zone DOUBLE PRECISION DEFAULT 0.0,                          /* UI Operator Target adjustment in the Lateral Direction for Heliostats in this Zone(m) */
  dZTarget_Zone DOUBLE PRECISION DEFAULT 0.0,                          /* UI Operator Target adjustment in the Vertical Direction for Heliostats in this Zone(m) */
  State_Zone INTEGER DEFAULT 0,                                        /* The status of the zone. */
    WashInProgress_Zone BOOLEAN DEFAULT false                          /* There are heliostats being washed in this zone */
);
  
/* Heliostats */
CREATE TABLE Heliostat(
  Instance_Heliostat INTEGER PRIMARY KEY,                              /* Primary Key for the Heliostat Table */
  AngleXAxisAdjustment_Heliostat DOUBLE PRECISION DEFAULT 0.0,         /* Northward Tilt of the post (radians) */
  AngleYAxisAdjustment_Heliostat DOUBLE PRECISION DEFAULT 0.0,         /* Eastward Tilt of the post (radians) */
  AngleZAxisAdjustment_Heliostat DOUBLE PRECISION DEFAULT 0.0,         /* Azimuthal Home Position Bias (radians) */
  AzWheelAdjustment_Heliostat DOUBLE PRECISION DEFAULT 0.0,            /* Azimuthal Gain Adjustment (percent) */
  ElWheelAdjustment_Heliostat DOUBLE PRECISION DEFAULT 0.0,            /* Elevation Gain Adjustment (percent) */
  ElOffsetAdjustment_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* Elevation Home Position Bias (radians) */
  AxisNonOrthogonality_Heliostat DOUBLE PRECISION DEFAULT 0.0,         /* Torque tube and post non-orthogonality (radians) */
  BoreSightMisalignment_Heliostat DOUBLE PRECISION DEFAULT 0.0,        /* Misalignment of the boresight to the mirror normal (radians) */
  NumGoodImages_Heliostat INTEGER DEFAULT 0,                           /* The number of good BCS captures (camera or manual) for a heliostat */
  NumGoodImagesFromLast_Heliostat INTEGER DEFAULT 0,                   /* Starting at the last BCS capture, the number of good recent captures (camera or manual) */
  NumBadImages_Heliostat INTEGER DEFAULT 0,                            /* The number of bad BCS captures (camera or manual) for a heliostat */
  RelaxCorrectionLimits_Heliostat INTEGER DEFAULT 0,                   /* Constraint relaxiation during estimation of BCS correction factors */
    RelaxAngleXAxis_Heliostat BOOLEAN DEFAULT false,                   /* Relax the Northward Tilt constraint */
    RelaxAngleYAxis_Heliostat BOOLEAN DEFAULT false,                   /* Relax the Eastward Tilt constraint */
    RelaxAngleZAxis_Heliostat BOOLEAN DEFAULT false,                   /* Relax the Azimuthal Home position bias constraint */
    RelaxAzWheel_Heliostat BOOLEAN DEFAULT false,                      /* Relax the Azimuthal Gain Adjustment constraint */
    RelaxElWheel_Heliostat BOOLEAN DEFAULT false,                      /* Relax the Elevation Gain Adjustment constraint */
    RelaxElOffset_Heliostat BOOLEAN DEFAULT false,                     /* Relax the Elevation Home position bias constraint */
    RelaxOrthogonality_Heliostat BOOLEAN DEFAULT false,                /* Torque tube and post non-orthogonality constraint */
    RelaxBoresightAlign_Heliostat BOOLEAN DEFAULT false,               /* Misalignment of the boresight to the mirror normal constraing */
  P$BCSHistory_Heliostat INTEGER DEFAULT 0,                            /* Pointer to the start of BCSHistory entries for the heliostat */
  I$BCSHistory_Heliostat INTEGER DEFAULT 0,                            /* The last BCSHistory entry created for the heliostat. */
  NumBCSHistory_Heliostat INTEGER DEFAULT 0,                           /* The total number of BCSHistory entries for the heliostat */
  AutoBCS$Heliostat_Heliostat INTEGER DEFAULT 0,                       /* Next Heliostat eleigible for Automatic BCS */
  FocusCalculation_Heliostat INTEGER DEFAULT 0,                        /* The methodology used to calculate the centroid of an image for the heliostat */
    FocusEllipseCenter_Heliostat BOOLEAN DEFAULT false,                /* Use the geometric center of the ellipse fitted to the image outline. */
    FocusEllipseCentroid_Heliostat BOOLEAN DEFAULT false,              /* Use the centroid of the ellipse fitted to the image outline. */
  P$Ring_Heliostat INTEGER DEFAULT 0,                                  /* The ring in which the helistat is situated */
  AngleAzFluxModel_Heliostat DOUBLE PRECISION DEFAULT 0.0,             /* The azimuthal angle of the heliostats used in the flux model (degrees) */
  AngleElFluxModel_Heliostat DOUBLE PRECISION DEFAULT 0.0,             /* The elevation angle of the heliostats used in the flux model (degrees) */
  ZaimFluxModel_Heliostat DOUBLE PRECISION DEFAULT 0.0,                /* The distance from the receiver centerline of the receiver aimpoints (m) */
  ID_Heliostat VARCHAR(32) DEFAULT '',                                 /* The name of a heliostat */
  Latitude_Heliostat DOUBLE PRECISION DEFAULT 0.0,                     /* The surveyed WGS84 latitude of the heliostat location (degrees) */
  Longitude_Heliostat DOUBLE PRECISION DEFAULT 0.0,                    /* The surveyed WGS84 longitude of the heliostat location  (degrees) */
  Elevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,                    /* The surveyed WGS84 elevation of the heliostat location (m) */
  XLocation_Heliostat DOUBLE PRECISION DEFAULT 0.0,                    /* The ENU East coordinate of the heliostat foundation (m) */
  YLocation_Heliostat DOUBLE PRECISION DEFAULT 0.0,                    /* The ENU North coordinate of the heliostat foundation (m) */
  ZLocation_Heliostat DOUBLE PRECISION DEFAULT 0.0,                    /* The ENU Up coordinate of the heliostat foundation (m) */
  dLatitude_Heliostat DOUBLE PRECISION DEFAULT 0.0,                    /* The surveyed Northing coordinate of the Heliostat foundation (us survey feet) */
  dLongitude_Heliostat DOUBLE PRECISION DEFAULT 0.0,                   /* The surveyed Easting coordinate of the Heliostat foundation (us survey feet) */
  dZBase_Heliostat DOUBLE PRECISION DEFAULT 0.0,                       /* The height of the concrete base to which the post is attaches (m) */
  XAimTower_Heliostat DOUBLE PRECISION DEFAULT 0.0,                    /* The ENU East coordinate of the aim point on the receiver (m) */
  YAimTower_Heliostat DOUBLE PRECISION DEFAULT 0.0,                    /* The ENU North coordinate of the aim point on the receiver (m) */
  ZAimTower_Heliostat DOUBLE PRECISION DEFAULT 0.0,                    /* The ENU Up coordinate of the aim point on the receiver (m) */
  XAimStandby_Heliostat DOUBLE PRECISION DEFAULT 0.0,                  /* The ENU East coordinate of the standby aimpoint (m) */
  YAimStandby_Heliostat DOUBLE PRECISION DEFAULT 0.0,                  /* The ENU North coordinate of the standby aimpoint (m) */
  ZAimStandby_Heliostat DOUBLE PRECISION DEFAULT 0.0,                  /* The ENU Up coordinate of the standby aimpoint (m) */
  XAimBCS_Heliostat DOUBLE PRECISION DEFAULT 0.0,                      /* The ENU East coordinate of the BCS calibration aimpoint (m) */
  YAimBCS_Heliostat DOUBLE PRECISION DEFAULT 0.0,                      /* The ENU North coordinate of the BCS calibration aimpoint (m) */
  ZAimBCS_Heliostat DOUBLE PRECISION DEFAULT 0.0,                      /* The ENU Up coordinate of the BCS calibration aimpoint (m) */
  AngleIdealAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,          /* The ideal azimuthal Angle SetPoint for the heliostat (degrees) */
  AngleSPAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,             /* The azimuthal Angle SetPoint for the heliostat (includes the bias) (degrees) */
  AngleAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,               /* The actual azimuthal reported Angle for the heliostat  (degrees) */
  GainAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,                /* The gain for azimuthal encoder    (counts/degree) */
  OffsetAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,              /* The offset for azimuthal encoder  (counts) */
  AngleFlipAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The azimuthal angle limit to apply the sign shift and prevent a 360 degree movement(degrees) */
  SignShiftAzimuthal_Heliostat INTEGER DEFAULT 0,                      /* The sign of a 360 degree shift needed to prevent a 360 degree movement [-1, 0, 1] */
  AngleHighAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The upper Angle azimuthal limit (degrees) */
  AngleLowAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,            /* The lower Angle azimuthal limit (degrees) */
  AngleScramAz_Heliostat DOUBLE PRECISION DEFAULT 0.0,                 /* The azimuthal scram Position for the heliostat(degrees) */
  AngleStopAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The azimuthal angle when the heliostat has an emergency stop or stow (degrees) */
  AngleWashAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The azimuthal angle when the heliostat has an emergency stop or stow (degrees) */
  AngleHomeAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The home position angle of the heliostat (degrees) */
  AngleStowAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The home position angle of the heliostat (degrees) */
  AngleIdealElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,          /* The ideal altitude Angle SetPoint for the heliostat (degrees) */
  AngleSPElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,             /* The Angle SetPoint for the heliostat (includes the bias) (degrees) */
  AngleElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,               /* The actual elevation reported Angle for the heliostat (degrees) */
  GainElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,                /* The gain for azimuthal encoder (counts/degree) */
  OffsetElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,              /* The offset for azimuthal encoder (counts) */
  AngleHighElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The upper Angle Elevation limit (degrees) */
  AngleLowElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,            /* The lower Angle Elevation limit (degrees) */
  AngleScramEl_Heliostat DOUBLE PRECISION DEFAULT 0.0,                 /* The elevation scram Position for the heliostat(degrees) */
  AngleStopElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The Elevation angle when the heliostat has an emergency stop or stow (degrees) */
  AngleWashElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The Elevation angle when the heliostat is in wash position (degrees) */
  AngleHomeElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The home elevation angle of the heliostat (degrees) */
  AngleStowElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The stow elevation angle of the heliostat (degrees) */
  EncoderElCmd_Heliostat INTEGER DEFAULT 0,                            /* The Elevation encoder counts (32-bits) going to the Heliostat (counts) */
  EncoderAzCmd_Heliostat INTEGER DEFAULT 0,                            /* The Azimuthal encoder counts (32-bits) going to the Heliostat (counts) */
  AngleElCmdDeg_Heliostat DOUBLE PRECISION DEFAULT 0.0,                /* The Elevation angle command to the Heliostat (degrees) */
  AngleAzCmdDeg_Heliostat DOUBLE PRECISION DEFAULT 0.0,                /* The Azimuthal angle command to the Heliostat (degrees) */
  RegisterRequested_Heliostat INTEGER DEFAULT 0,                       /* The register whose contents are requested from the heliostat */
  EncoderElData_Heliostat INTEGER DEFAULT 0,                           /* The Elevation encoder counts (32-bits) coming from the Heliostat (counts) */
  EncoderAzData_Heliostat INTEGER DEFAULT 0,                           /* The Azimuthal encoder counts (32-bits) coming from the Heliostat (counts) */
  AngleElDataDeg_Heliostat DOUBLE PRECISION DEFAULT 0.0,               /* The converted Elevation angle coming from the Heliostat (degrees) */
  AngleAzDataDeg_Heliostat DOUBLE PRECISION DEFAULT 0.0,               /* The converted Azimuthal angle coming from the Heliostat (degrees) */
  SpeedElDegPerMin_Heliostat DOUBLE PRECISION DEFAULT 0.0,             /* The Elevation speed calculated from the Heliostat data (degrees/minute) */
  SpeedAzDegPerMin_Heliostat DOUBLE PRECISION DEFAULT 0.0,             /* The Azimuthal speed calculated from the Heliostat data (degrees/minute) */
  RegisterReturned_Heliostat INTEGER DEFAULT 0,                        /* The register whose contents were returned from the heliostat */
  ValueRegister_Heliostat INTEGER DEFAULT 0,                           /* The value of the register returned from the heliostat */
  StatusAzSet_Heliostat INTEGER DEFAULT 0,                             /* The UI Operator can set the commanded bits for the Az Drive when in Analyst Mode. */
    MarkAzSet_Heliostat BOOLEAN DEFAULT false,                         /* The operator has set the Seekmark Az Drive command */
    FastAzSet_Heliostat BOOLEAN DEFAULT false,                         /* The operator has set the Fast Az Drive command */
    SlowAzSet_Heliostat BOOLEAN DEFAULT false,                         /* The operator has set the Slow Az Drive command */
    DirectedPositionAzSet_Heliostat BOOLEAN DEFAULT false,             /* The operator has set the Az Drive Directed Position command */
    OffLineAzSet_Heliostat BOOLEAN DEFAULT false,                      /* The operator has set the Az Drive Offline command */
    ResetAzSet_Heliostat BOOLEAN DEFAULT false,                        /* The operator has set the Az Drive Reset command */
    UpdateScramPosnAzSet_Heliostat BOOLEAN DEFAULT false,              /* The operator has set the Update Az Drive Scram Position command */
    ReportRegisterSet_Heliostat BOOLEAN DEFAULT false,                 /* The operator has set the Report Register command */
    ResetFaultAzSet_Heliostat BOOLEAN DEFAULT false,                   /* The operator has set the Az Drive Reset Fault command */
    InitializeAzSet_Heliostat BOOLEAN DEFAULT false,                   /* The operator has set the Az Drive Initialize command */
  StatusElSet_Heliostat INTEGER DEFAULT 0,                             /* The UI Operator can set the commanded bits for the El Drive when in Analyst Mode. */
    MarkElSet_Heliostat BOOLEAN DEFAULT false,                         /* The operator has set the Seekmark El Drive command */
    FastElSet_Heliostat BOOLEAN DEFAULT false,                         /* The operator has set the Fast El Drive command */
    SlowElSet_Heliostat BOOLEAN DEFAULT false,                         /* The operator has set the Slow El Drive command */
    DirectedPositionElSet_Heliostat BOOLEAN DEFAULT false,             /* The operator has set the El Drive Directed Position command */
    OffLineElSet_Heliostat BOOLEAN DEFAULT false,                      /* The operator has set the El Drive Offline command */
    ResetElSet_Heliostat BOOLEAN DEFAULT false,                        /* The operator has set the El Drive Reset command */
    UpdateScramPosnElSet_Heliostat BOOLEAN DEFAULT false,              /* The operator has set the Update El Drive Scram Position command */
    Reserved07ElSet_Heliostat BOOLEAN DEFAULT false,                   /* The operator has set the 7th reserved bit in the El Drive command */
    ResetFaultElSet_Heliostat BOOLEAN DEFAULT false,                   /* The operator has set the El Drive Reset Fault command */
    InitializeElSet_Heliostat BOOLEAN DEFAULT false,                   /* The operator has set the El Drive Initialize command */
  StatusSet_Heliostat INTEGER DEFAULT 0,                               /* The UI operator can issue the following commands for the heliostat. */
    CopyCommandBits_Heliostat BOOLEAN DEFAULT false,                   /* Send the StatusAzSet and StatusElSet command words to the heliostat */
    RefreshConfiguration_Heliostat BOOLEAN DEFAULT false,              /* Reread the configuration data */
    RefreshConversionData_Heliostat BOOLEAN DEFAULT false,             /* Reread the Raw to Engineering Conversion Gains and Offsets */
    ManualBCS_Heliostat BOOLEAN DEFAULT false,                         /* Perform a manual BCS data capture */
    LaserFocusedOnTarget_Heliostat BOOLEAN DEFAULT false,              /* Perform a Laser Focus */
    SelectStow_Heliostat BOOLEAN DEFAULT false,                        /* Command the heliostat to Wind Stow */
    SelectSleep_Heliostat BOOLEAN DEFAULT false,                       /* Command the heliostat to Night Stow */
    SelectTrack_Heliostat BOOLEAN DEFAULT false,                       /* Command the heliostat to Track */
    SelectDefocus_Heliostat BOOLEAN DEFAULT false,                     /* Command the heliostat to Emergency Defocus */
    SelectStandby_Heliostat BOOLEAN DEFAULT false,                     /* Command the heliostat to Standby */
    SelectWashPosition_Heliostat BOOLEAN DEFAULT false,                /* Command the heliostat to Wash Position */
    SelectRainPosition_Heliostat BOOLEAN DEFAULT false,                /* Command the heliostat to Rainwash */
    SelectDefrost_Heliostat BOOLEAN DEFAULT false,                     /* Command the heliostat to Defrost */
    SelectStop_Heliostat BOOLEAN DEFAULT false,                        /* Command the heliostat to Stop */
    SelectManualPosition_Heliostat BOOLEAN DEFAULT false,              /* Command the heliostat to Manual Position */
    SelectBCSPosition_Heliostat BOOLEAN DEFAULT false,                 /* Command the heliostat to BCS Calibration */
    SelectSeekMarkAz_Heliostat BOOLEAN DEFAULT false,                  /* Command the heliostat return to azimuthal home position */
    SelectSeekMarkEl_Heliostat BOOLEAN DEFAULT false,                  /* Command the heliostat return to elevation home position */
    SelectWashComplete_Heliostat BOOLEAN DEFAULT false,                /* Record wash complete for the heliostat */
    SelectOperator_Heliostat BOOLEAN DEFAULT false,                    /* Command the heliostat to operator control mode */
    SelectAnalyst_Heliostat BOOLEAN DEFAULT false,                     /* Command the heliostat to analyst control mode */
    SelectAutomatic_Heliostat BOOLEAN DEFAULT false,                   /* Command the heliostat to automatic control mode */
    SelectStart_Heliostat BOOLEAN DEFAULT false,                       /* Select this as the starting heliostat for (ring, hfc, zone depending on display issued from) */
    SelectEnd_Heliostat BOOLEAN DEFAULT false,                         /* Select this as the ending heliostat for (ring, hfc, zone depending on display issued from) */
  StatusAzOut_Heliostat INTEGER DEFAULT 0,                             /* The azimuth status command word going to the Heliostat */
    MarkAzOut_Heliostat BOOLEAN DEFAULT false,                         /* Seekmark Az Drive command */
    FastAzOut_Heliostat BOOLEAN DEFAULT false,                         /* Fast Az Drive command */
    SlowAzOut_Heliostat BOOLEAN DEFAULT false,                         /* Slow Az Drive command */
    DirectedPositionAzOut_Heliostat BOOLEAN DEFAULT false,             /* Az Drive Directed Position command */
    OffLineAzOut_Heliostat BOOLEAN DEFAULT false,                      /* Az Drive Offline command */
    ResetAzOut_Heliostat BOOLEAN DEFAULT false,                        /* Az Drive Reset command */
    UpdateScramPosnAzOut_Heliostat BOOLEAN DEFAULT false,              /* Update Az Drive Scram Position command */
    ReportRegisterOut_Heliostat BOOLEAN DEFAULT false,                 /* Report Register command */
    ResetFaultAzOut_Heliostat BOOLEAN DEFAULT false,                   /* Az Drive Reset Fault command */
    InitializeAzOut_Heliostat BOOLEAN DEFAULT false,                   /* Az Drive Initialize command */
  StatusElOut_Heliostat INTEGER DEFAULT 0,                             /* The elevation status command word (16-bits) going to the Heliostat */
    MarkElOut_Heliostat BOOLEAN DEFAULT false,                         /* Seekmark El Drive command */
    FastElOut_Heliostat BOOLEAN DEFAULT false,                         /* Fast El Drive command */
    SlowElOut_Heliostat BOOLEAN DEFAULT false,                         /* Slow El Drive command */
    DirectedPositionElOut_Heliostat BOOLEAN DEFAULT false,             /* El Drive Directed Position command */
    OffLineElOut_Heliostat BOOLEAN DEFAULT false,                      /* El Drive Offline command */
    ResetElOut_Heliostat BOOLEAN DEFAULT false,                        /* El Drive Reset command */
    UpdateScramPosnElOut_Heliostat BOOLEAN DEFAULT false,              /* Update El Drive Scram Position command */
    Reserved07ElOut_Heliostat BOOLEAN DEFAULT false,                   /* Spare */
    ResetFaultElOut_Heliostat BOOLEAN DEFAULT false,                   /* El Drive Reset Fault command */
    InitializeElOut_Heliostat BOOLEAN DEFAULT false,                   /* El Drive Initialize command */
  StatusAzIn_Heliostat INTEGER DEFAULT 0,                              /* The azimuth status word (16-bits) coming from the Heliostat */
    MarkAzIn_Heliostat BOOLEAN DEFAULT false,                          /* The drive is seeking its home position */
    FastAzIn_Heliostat BOOLEAN DEFAULT false,                          /* The drive is in fast mode */
    SlowAzIn_Heliostat BOOLEAN DEFAULT false,                          /* The drive is in slow mode */
    DirectedPositionAzIn_Heliostat BOOLEAN DEFAULT false,              /* The drive is in directed position */
    OffLineAzIn_Heliostat BOOLEAN DEFAULT false,                       /* The drive is offline */
    ErrorAzIn_Heliostat BOOLEAN DEFAULT false,                         /* The drive reports an error */
    RegisterReportIn_Heliostat BOOLEAN DEFAULT false,                  /* The drive is reporting the contents of a register */
    TemperatureHighAz_Heliostat BOOLEAN DEFAULT false,                 /* The drive temperature is above the warning limit but below the redline. */
    TemperatureRedlineAz_Heliostat BOOLEAN DEFAULT false,              /* The drive temperature has exceeded the redline. */
    CurrentHighAz_Heliostat BOOLEAN DEFAULT false,                     /* The drive current is above the warning limit but below the redline. */
    CurrentRedlineAz_Heliostat BOOLEAN DEFAULT false,                  /* The drive current has exceeded the redline. */
    InitializedAz_Heliostat BOOLEAN DEFAULT false,                     /* The drive is initializing */
    EncoderConfidenceAz_Heliostat BOOLEAN DEFAULT false,               /* The encoder confidence is good */
    ProximitySensorAz_Heliostat BOOLEAN DEFAULT false,                 /* The proximity sensor is high */
    NoScramPositions_Heliostat BOOLEAN DEFAULT false,                  /* No scram position has been received. */
    Reserved15AzIn_Heliostat BOOLEAN DEFAULT false,                    /* Reserved bit 15. */
  StatusElIn_Heliostat INTEGER DEFAULT 0,                              /* The elevation status word (16-bits) coming from the Heliostat */
    MarkElIn_Heliostat BOOLEAN DEFAULT false,                          /* The drive is seeking its home position */
    FastElIn_Heliostat BOOLEAN DEFAULT false,                          /* The drive is in fast mode */
    SlowElIn_Heliostat BOOLEAN DEFAULT false,                          /* The drive is in slow mode */
    DirectedPositionElIn_Heliostat BOOLEAN DEFAULT false,              /* The drive is in directed position */
    OffLineElIn_Heliostat BOOLEAN DEFAULT false,                       /* The drive is offline */
    ErrorElIn_Heliostat BOOLEAN DEFAULT false,                         /* The drive reports an error */
    Reserved06ElIn_Heliostat BOOLEAN DEFAULT false,                    /* Bit 6 is reserved */
    TemperatureHighEl_Heliostat BOOLEAN DEFAULT false,                 /* The drive temperature is above the warning limit but below the redline. */
    TemperatureRedlineEl_Heliostat BOOLEAN DEFAULT false,              /* The drive temperature has exceeded the redline. */
    CurrentHighEl_Heliostat BOOLEAN DEFAULT false,                     /* The drive current is above the warning limit but below the redline. */
    CurrentRedlineEl_Heliostat BOOLEAN DEFAULT false,                  /* The drive current has exceeded the redline. */
    InitializedEl_Heliostat BOOLEAN DEFAULT false,                     /* The drive is initializing */
    EncoderConfidenceEl_Heliostat BOOLEAN DEFAULT false,               /* The encoder confidence is good */
    ProximitySensorEl_Heliostat BOOLEAN DEFAULT false,                 /* The proximity sensor is high */
    LimitSwitchUpperEl_Heliostat BOOLEAN DEFAULT false,                /* The upper limit position sensor is high */
    LimitSwitchLowerEl_Heliostat BOOLEAN DEFAULT false,                /* The lower limit position sensor is high */
  FaultCodeAzimuthal_Heliostat INTEGER DEFAULT 0,                      /* Azimuthal Drive Fault Code */
  FaultCodeElevation_Heliostat INTEGER DEFAULT 0,                      /* Elevation Drive Fault Code */
  Device_Heliostat INTEGER DEFAULT 0,                                  /* The serial bus device id of the heliostat */
  I$Htype_Heliostat INTEGER DEFAULT 0,                                 /* Indirect pointer to Heliostat Type (Htype) */
  P$Zone_Heliostat INTEGER DEFAULT 0,                                  /* Indirect pointer to zone of this heliostat */
  AngleManualAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,         /* The UI operator entered azimuthal Angle for the heliostat (Degrees) */
  AngleBiasAzimuthal_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The UI Operator entered azimuthal Angle bias of the heliostat (Degrees) */
  AngleManualElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,         /* The UI Operator entered elevation Angle for the heliostat (Degrees) */
  AngleBiasElevation_Heliostat DOUBLE PRECISION DEFAULT 0.0,           /* The UI Operator entered elevation Angle bias of the heliostat (Degrees) */
  dXTarget_Heliostat DOUBLE PRECISION DEFAULT 0.0,                     /* Target adjustment in the Lateral Direction for the Heliostat(m) */
  dZTarget_Heliostat DOUBLE PRECISION DEFAULT 0.0,                     /* Target adjustment in the Vertical Direction for the Heliostat(m) */
  Command_Heliostat INTEGER DEFAULT 0,                                 /* The commanded state and mode of the heliostat */
    StowCommand_Heliostat BOOLEAN DEFAULT false,                       /* The Heliostat has been commanded to the Wind Stow State */
    SleepCommand_Heliostat BOOLEAN DEFAULT false,                      /* The Heliostat has been commanded to the Night Stow State */
    TrackCommand_Heliostat BOOLEAN DEFAULT false,                      /* The Heliostat has been commanded to the Track State */
    DefocusCommand_Heliostat BOOLEAN DEFAULT false,                    /* The Heliostat has been commanded to the Emergency Defocus State */
    StandbyCommand_Heliostat BOOLEAN DEFAULT false,                    /* The Heliostat has been commanded to the Standby State */
    BCSPositionCommand_Heliostat BOOLEAN DEFAULT false,                /* The Heliostat has been commanded to the BCS Position State */
    ManualCommand_Heliostat BOOLEAN DEFAULT false,                     /* The Heliostat has been commanded to the Manual Position State */
    WashPositionCommand_Heliostat BOOLEAN DEFAULT false,               /* The Heliostat has been commanded to the Wash State */
    RainPositionCommand_Heliostat BOOLEAN DEFAULT false,               /* The Heliostat has been commanded to the Rainwash State */
    DefrostCommand_Heliostat BOOLEAN DEFAULT false,                    /* The Heliostat has been commanded to the Defrost State */
    StopCommand_Heliostat BOOLEAN DEFAULT false,                       /* The Heliostat has been commanded to Stop */
    RecordWashCommand_Heliostat BOOLEAN DEFAULT false,                 /* The Heliostat has been commanded to record the TimeLastWashed */
    BCSCaptureCommand_Heliostat BOOLEAN DEFAULT false,                 /* The Heliostat has been commanded to record a Manual BCS history */
    CalibrateAzimuthal_Heliostat BOOLEAN DEFAULT false,                /* The Heliostat has been commanded to perform Az Drive Calibration */
    CalibrateElevation_Heliostat BOOLEAN DEFAULT false,                /* The Heliostat has been commanded to perform El Drive Calibration */
    OperatorControl_Heliostat BOOLEAN DEFAULT false,                   /* The Heliostat has been commanded to the Operator Control Mode */
    AutomaticControl_Heliostat BOOLEAN DEFAULT false,                  /* The Heliostat has been commanded to the Automatic Control Mode */
    AnalystControl_Heliostat BOOLEAN DEFAULT false,                    /* The Heliostat has been commanded to the Analyst  Control Mode */
    FastOption_Heliostat BOOLEAN DEFAULT false,                        /* The Heliostat is allowed to exceed the Fast Speed limits */
    ModeTest_Heliostat BOOLEAN DEFAULT false,                          /* The heliostat is in test mode.  This enables internal diagnostic output. */
    KeepOutZoneAzimuthal_Heliostat BOOLEAN DEFAULT false,              /* This heliostat has a Keep-Out Zone for the Az Drive */
    KeepOutZoneElevation_Heliostat BOOLEAN DEFAULT false,              /* This heliostat has a Keep-Out Zone for the El Drive */
    DisableBCSProcessing_Heliostat BOOLEAN DEFAULT false,              /* This heliostat will not be scheduled for BCS. */
  State_Heliostat INTEGER DEFAULT 0,                                   /* The actual state of the heliostat */
    Stow_Heliostat BOOLEAN DEFAULT false,                              /* The Heliostat is transitioning to the Wind Stow State */
    Stowed_Heliostat BOOLEAN DEFAULT false,                            /* The Heliostat has transitioned to the Wind Stow State */
    Sleep_Heliostat BOOLEAN DEFAULT false,                             /* The Heliostat is transitioning to the Night Stow State */
    Sleeping_Heliostat BOOLEAN DEFAULT false,                          /* The Heliostat has transitioned to the Night Stow State */
    Track_Heliostat BOOLEAN DEFAULT false,                             /* The Heliostat is transitioning to the Track State */
    Tracking_Heliostat BOOLEAN DEFAULT false,                          /* The Heliostat has transitioned to the Track State */
    Defocus_Heliostat BOOLEAN DEFAULT false,                           /* The Heliostat is transitioning to the Emergency Defocus State */
    Defocused_Heliostat BOOLEAN DEFAULT false,                         /* The Heliostat has transitioned to the Emergency Defocus State */
    StandBy_Heliostat BOOLEAN DEFAULT false,                           /* The Heliostat is transitioning to the Standby State */
    AtStandBy_Heliostat BOOLEAN DEFAULT false,                         /* The Heliostat has transitioned to the Standby State */
    WashPosition_Heliostat BOOLEAN DEFAULT false,                      /* The Heliostat is transitioning to the Wash State */
    AtWashPosition_Heliostat BOOLEAN DEFAULT false,                    /* The Heliostat has transitioned to the Wash State */
    RainWashPosition_Heliostat BOOLEAN DEFAULT false,                  /* The Heliostat is transitioning to the Rainwash State */
    AtRainWashPosition_Heliostat BOOLEAN DEFAULT false,                /* The Heliostat has transitioned to the Rainwash State */
    Defrost_Heliostat BOOLEAN DEFAULT false,                           /* The Heliostat is transitioning to the Defrost State */
    AtDefrost_Heliostat BOOLEAN DEFAULT false,                         /* The Heliostat has transitioned to the Defrost State */
    Stop_Heliostat BOOLEAN DEFAULT false,                              /* The Heliostat is stopping */
    Stopped_Heliostat BOOLEAN DEFAULT false,                           /* The Heliostat has stoped */
    BCSPosition_Heliostat BOOLEAN DEFAULT false,                       /* The Heliostat is transitioning to the BCS Position State */
    AtBCSPosition_Heliostat BOOLEAN DEFAULT false,                     /* The Heliostat has transitioned to the BCS Position State */
    BCSCheck_Heliostat BOOLEAN DEFAULT false,                          /* The Heliostat is being checked to ensure it is holding the BCS Position */
    BCSChecked_Heliostat BOOLEAN DEFAULT false,                        /* The Heliostat has been checked and is holding the BCS Position */
    SeekMarkAzimuth_Heliostat BOOLEAN DEFAULT false,                   /* The heliostat Az Drive is moving to its home position */
    SeekMarkElevation_Heliostat BOOLEAN DEFAULT false,                 /* The heliostat El Drive is moving to its home position */
    ManualPosition_Heliostat BOOLEAN DEFAULT false,                    /* The Heliostat is transitioning to the Manual Position State */
    AtManualPosition_Heliostat BOOLEAN DEFAULT false,                  /* The Heliostat has transitioning to the Manual Position State */
    LossOfPower_Heliostat BOOLEAN DEFAULT false,                       /* The Heliostat has lost the power feed to move the drive motors */
    ModeManual_Heliostat BOOLEAN DEFAULT false,                        /* The Heliostat is under operator control */
    ModeAutomatic_Heliostat BOOLEAN DEFAULT false,                     /* The Heliostat is under system control */
    UseBCSCorrections_Heliostat BOOLEAN DEFAULT false,                 /* Use the BCS correction parameters to adjust the Azimuth and Elevation Angle SetPoints */
    FailedInitializeAz_Heliostat BOOLEAN DEFAULT false,                /* The heliostat Az Drive failed to complete initialization within the allotted time. */
    FailedInitializeEl_Heliostat BOOLEAN DEFAULT false,                /* The heliostat El Drive failed to complete initialization within the allotted time. */
  TactileNavigation_Heliostat INTEGER DEFAULT 0,                       /* The UI Operator controls the speed and direction of the Heliostat for Manual BCS */
    SlowAzRight_Heliostat BOOLEAN DEFAULT false,                       /* UI Operator command to move the Az Drive slowly to the right. */
    SlowAzLeft_Heliostat BOOLEAN DEFAULT false,                        /* UI Operator command to move the Az Drive slowly to the left */
    FastAzRight_Heliostat BOOLEAN DEFAULT false,                       /* UI Operator command to move the Az Drive quickly to the right. */
    FastAzLeft_Heliostat BOOLEAN DEFAULT false,                        /* UI Operator command to move the Az Drive quickly to the right. */
    SlowElUp_Heliostat BOOLEAN DEFAULT false,                          /* UI Operator command to move the El Drive slowly up. */
    SlowElDown_Heliostat BOOLEAN DEFAULT false,                        /* UI Operator command to move the El Drive slowly down. */
    FastElUp_Heliostat BOOLEAN DEFAULT false,                          /* UI Operator command to move the El Drive quickly up. */
    FastElDown_Heliostat BOOLEAN DEFAULT false,                        /* UI Operator command to move the El Drive quickly down. */
  HoldPosition_Heliostat INTEGER DEFAULT 0,                            /* UI Operator command to the heliostat to hold its position */
    HoldActualPosition_Heliostat BOOLEAN DEFAULT false,                /* Hold the current Azimuthal and Elevation Angle Positions */
    HoldCommonPosition_Heliostat BOOLEAN DEFAULT false,                /* Hold the current Azimuthal and Elevation Angle Positions of the selected starting heliostat */
    HoldManualPosition_Heliostat BOOLEAN DEFAULT false,                /* Hold the manual Azimuthal and Elevation Angle Positions */
  RepairStatus_Heliostat INTEGER DEFAULT 0,                            /* UI Operator set this when the heliostat needs a repair */
    RepairNeeded_Heliostat BOOLEAN DEFAULT false,                      /* Flag to help direct heliostat repairs and triage BCS priorities */
  TimeRepairRequested_Heliostat TIMESTAMPTZ DEFAULT now(),             /* The time when the repair was requested */
  TimeRepairCompleted_Heliostat TIMESTAMPTZ DEFAULT now(),             /* The time when the repair was completed */
  TimeInitializedAz_Heliostat TIMESTAMPTZ DEFAULT now(),               /* The last times the Az Drive was initialized from the HFCS */
  TimeInitializedEl_Heliostat TIMESTAMPTZ DEFAULT now(),               /* The last times the El Drive was initialized from the HFCS */
  TimeTagLastBCS_Heliostat TIMESTAMPTZ DEFAULT now(),                  /* The last time a BCS capture was done for this heliostat */
  TimeLastWashed_Heliostat TIMESTAMPTZ DEFAULT now(),                  /* The last time the heliostat was washed */
  I$HCU_Heliostat INTEGER DEFAULT 0,                                   /* The heliostat controller of the heliostat */
  IOneLine_Heliostat INTEGER DEFAULT 0,                                /* The lateral coordinate of the heliostat in the oneline display (pixels) */
  JOneLine_Heliostat INTEGER DEFAULT 0,                                /* The vertical coordinate of the heliostat in the oneline display (pixels) */
  IMapBoard_Heliostat INTEGER DEFAULT 0,                               /* The lateral coordinate of the heliostat in the map board (pixels) */
  JMapBoard_Heliostat INTEGER DEFAULT 0,                               /* The lateral coordinate of the heliostat in the map board (pixels) */
  KKSNAME_Heliostat VARCHAR(15) DEFAULT ''                             /* The contractor name of the heliostat */
);
  
/* Thermocouples */
CREATE TABLE Thermocouple(
  Instance_Thermocouple INTEGER PRIMARY KEY,                           /* Primary Key for the Thermocouple Table */
  ID_Thermocouple VARCHAR(32) DEFAULT '',                              /* The name of a Thermocouple */
  I$RowPnl_Thermocouple INTEGER DEFAULT 0,                             /* The pointer to the thermocouple's associated rowpnl on the receiver */
  TemperatureF_Thermocouple DOUBLE PRECISION DEFAULT 0.0,              /* Temperature of the thermocouple (degF) */
  TemperatureC_Thermocouple DOUBLE PRECISION DEFAULT 0.0,              /* Temperature of the thermocouple (degC) */
  Temperature_Thermocouple DOUBLE PRECISION DEFAULT 0.0,               /* Temperature of the thermocouple (System Units) */
  StatusSet_Thermocouple INTEGER DEFAULT 0,                            /* Operator UI selected to remove or restore a thermocouple for the preheat calculations */
    SelectRemove_Thermocouple BOOLEAN DEFAULT false,                   /* The operator toggles this to true/false to remove/restore a thermocouple temperature in the calculations */
  Status_Thermocouple INTEGER DEFAULT 0,                               /* The status of the thermocouple measurement */
    Normal_Thermocouple BOOLEAN DEFAULT false,                         /* The measurement is good */
    Fault_Thermocouple BOOLEAN DEFAULT false,                          /* The measurement is bad */
    Replaced_Thermocouple BOOLEAN DEFAULT false,                       /* The measurement has been manually replaced */
  Protection_Thermocouple INTEGER DEFAULT 0,                           /* The thermocouple alarm status */
    ExceedsTripLimit_Thermocouple BOOLEAN DEFAULT false,               /* This thermocouple temperature is beyond the trip limit */
    ExceedsWarnLimit_Thermocouple BOOLEAN DEFAULT false,               /* This thermocouple temperature is beyond the warning limit */
    WithinLimits_Thermocouple BOOLEAN DEFAULT false,                   /* This thermocouple temperature is within limits */
  StrikesWarn_Thermocouple INTEGER DEFAULT 0,                          /* Number of times thermocouple exceeded warning limit */
  StrikesTrip_Thermocouple INTEGER DEFAULT 0                           /* Number of times thermocouple exceeded trip limit */
);
  
/* Pumps */
CREATE TABLE Pump(
  Instance_Pump INTEGER PRIMARY KEY,                                   /* Primary Key for the Pump Table */
  ID_Pump VARCHAR(32) DEFAULT '',                                      /* The name of a Pump */
  SpeedCurrent_Pump DOUBLE PRECISION DEFAULT 0.0,                      /* The speed of the pump (% of rated) */
  SpeedCurrentSetPoint_Pump DOUBLE PRECISION DEFAULT 0.0,              /* The speed setpoint of the pump (% of rated) */
  Status_Pump INTEGER DEFAULT 0,                                       /* Defines pump availability */
    OnLine_Pump BOOLEAN DEFAULT false,                                 /* Normal Operation */
    OffLine_Pump BOOLEAN DEFAULT false,                                /* Turned off */
    PumpFault_Pump BOOLEAN DEFAULT false,                              /* Fault condition reported */
  I$Flow_Pump INTEGER DEFAULT 0                                        /* Pointer to the pump flow rate transducer */
);
  
/* Fluid Tanks */
CREATE TABLE Tank(
  Instance_Tank INTEGER PRIMARY KEY,                                   /* Primary Key for the Tank Table */
  ID_Tank VARCHAR(32) DEFAULT '',                                      /* The name of a tank */
  Height_Tank DOUBLE PRECISION DEFAULT 0.0,                            /* The height of the tank (m) */
  Area_Tank DOUBLE PRECISION DEFAULT 0.0,                              /* The area of the tank (m^2) */
  Volume_Tank DOUBLE PRECISION DEFAULT 0.0,                            /* The tank volume (m^3) */
  ElevationBase_Tank DOUBLE PRECISION DEFAULT 0.0,                     /* The elevation of the tank base (meters) */
  LevelMeters_Tank DOUBLE PRECISION DEFAULT 0.0,                       /* The fill level of the tank (meters) */
  LevelFeet_Tank DOUBLE PRECISION DEFAULT 0.0,                         /* The fill level of the tank (feet */
  LevelPct_Tank DOUBLE PRECISION DEFAULT 0.0,                          /* The percent fill level of the tank */
  I$Pressure_Tank INTEGER DEFAULT 0,                                   /* Pointer to the pressure measurment of the tank */
  I$Thermocouple_Tank INTEGER DEFAULT 0,                               /* Pointer to the temperature measurement of the tank */
  Status_Tank INTEGER DEFAULT 0,                                       /* The status of the tank level measurement */
    Normal_Tank BOOLEAN DEFAULT false,                                 /* The tank level measurement is good */
    Fault_Tank BOOLEAN DEFAULT false,                                  /* The tank level measurement is bad */
    Replaced_Tank BOOLEAN DEFAULT false                                /* The tank level measurement has been manually replaced */
);
  
/* Valves */
CREATE TABLE Valve(
  Instance_Valve INTEGER PRIMARY KEY,                                  /* Primary Key for the Valve Table */
  ID_Valve VARCHAR(32) DEFAULT '',                                     /* The name of a valve */
  PositionCurrent_Valve DOUBLE PRECISION DEFAULT 0.0,                  /* The current position of the valve */
  PositionSetPoint_Valve DOUBLE PRECISION DEFAULT 0.0,                 /* The position setpoint of the valve */
  CoefProportional_Valve DOUBLE PRECISION DEFAULT 0.0,                 /* The valve PID proportional control coefficient */
  CoefIntegral_Valve DOUBLE PRECISION DEFAULT 0.0,                     /* The valve PID proportional control coefficient */
  CoefDifferential_Valve DOUBLE PRECISION DEFAULT 0.0,                 /* The valve PID differential control coefficient */
  Status_Valve INTEGER DEFAULT 0,                                      /* The status of the valve position measurement */
    Normal_Valve BOOLEAN DEFAULT false,                                /* The position measurement is good */
    Fault_Valve BOOLEAN DEFAULT false,                                 /* The position measurement is bad */
    Replaced_Valve BOOLEAN DEFAULT false,                              /* The position measurement has been manually replaced */
  I$Flow_Valve INTEGER DEFAULT 0                                       /* Pointer to the Valve flow rate transducer */
);
  
/* Pressure transducers */
CREATE TABLE Pressure(
  Instance_Pressure INTEGER PRIMARY KEY,                               /* Primary Key for the Pressure Table */
  ID_Pressure VARCHAR(32) DEFAULT '',                                  /* The name of a Pressure transducer */
  Ppsig_Pressure DOUBLE PRECISION DEFAULT 0.0,                         /* The pressure measurment value (psig) */
  Pkpa_Pressure DOUBLE PRECISION DEFAULT 0.0,                          /* The pressure measurement value (kpa) */
  P_Pressure DOUBLE PRECISION DEFAULT 0.0,                             /* Pressure measurement in system units (Both English and SI units will be contained in the database) */
  Status_Pressure INTEGER DEFAULT 0,                                   /* The status of the pressure measurement */
    Normal_Pressure BOOLEAN DEFAULT false,                             /* The pressure measurement is good */
    Fault_Pressure BOOLEAN DEFAULT false,                              /* The pressure measurement is bad */
    Replaced_Pressure BOOLEAN DEFAULT false                            /* The perssure measurement has been manually replaced */
);
  
/* Level Transducers */
CREATE TABLE Level(
  Instance_Level INTEGER PRIMARY KEY,                                  /* Primary Key for the Level Table */
  ID_Level VARCHAR(32) DEFAULT '',                                     /* The name of a Level transducer */
  Lfeet_Level DOUBLE PRECISION DEFAULT 0.0,                            /* The level value (feet) */
  Lmeters_Level DOUBLE PRECISION DEFAULT 0.0,                          /* The level values (meters) */
  Lmax_Level DOUBLE PRECISION DEFAULT 0.0,                             /* The maximum level (m) */
  L_Level DOUBLE PRECISION DEFAULT 0.0,                                /* The level in system units */
  Lpercent_Level DOUBLE PRECISION DEFAULT 0.0,                         /* The level in percent */
  Status_Level INTEGER DEFAULT 0,                                      /* The status of the level measurement */
    Normal_Level BOOLEAN DEFAULT false,                                /* The level measurement is good */
    Fault_Level BOOLEAN DEFAULT false,                                 /* The level measurement is bad */
    Replaced_Level BOOLEAN DEFAULT false                               /* The level measurement has been manually replaced */
);
  
/* Flow Meters */
CREATE TABLE Flow(
  Instance_Flow INTEGER PRIMARY KEY,                                   /* Primary Key for the Flow Table */
  ID_Flow VARCHAR(32) DEFAULT '',                                      /* The name of a Flow Meter */
  WKgPerSec_Flow DOUBLE PRECISION DEFAULT 0.0,                         /* Flow rate measurement (Kg/second) */
  WLbmsPerSec_Flow DOUBLE PRECISION DEFAULT 0.0,                       /* Flow rate measurement (Lbms/second) */
  WKpph_Flow DOUBLE PRECISION DEFAULT 0.0,                             /* Flow rate measurement (KiloLbms/hour) */
  W_Flow DOUBLE PRECISION DEFAULT 0.0,                                 /* Flow rate measurement (System units) */
  WFiltered_Flow DOUBLE PRECISION DEFAULT 0.0,                         /* Filtered flow rate measurement (System units) */
  Status_Flow INTEGER DEFAULT 0,                                       /* Status of the flow measurement */
    Normal_Flow BOOLEAN DEFAULT false,                                 /* The measurement is good */
    Fault_Flow BOOLEAN DEFAULT false,                                  /* The measurement is bad */
    Replaced_Flow BOOLEAN DEFAULT false                                /* The measurement has been manually replaced */
);
  
/* Heliostat Template */
CREATE TABLE Htype(
  Instance_Htype INTEGER PRIMARY KEY,                                  /* Primary Key for the Htype Table */
  ID_Htype VARCHAR(32) DEFAULT '',                                     /* The name of a heliostat Htype */
  Width_Htype DOUBLE PRECISION DEFAULT 0.0,                            /* Width of mirror including gaps (m) */
  Height_Htype DOUBLE PRECISION DEFAULT 0.0,                           /* Height of mirror including gaps (m) */
  HeightPost_Htype DOUBLE PRECISION DEFAULT 0.0,                       /* Height of the post (m) */
  Clearance_Htype DOUBLE PRECISION DEFAULT 0.0,                        /* Clearance between ground and bottom of mirror when vertical (m) */
  AreaGlass_Htype DOUBLE PRECISION DEFAULT 0.0,                        /* Glass Area (m^2) */
  WidthFacet_Htype DOUBLE PRECISION DEFAULT 0.0,                       /* Facet Width (m) */
  HeightFacet_Htype DOUBLE PRECISION DEFAULT 0.0,                      /* Facet Height (m) */
  GapFacet_Htype DOUBLE PRECISION DEFAULT 0.0,                         /* Gap Between Facets (m) */
  NumColumn_Htype INTEGER DEFAULT 0,                                   /* Number of Columns */
  NumRow_Htype INTEGER DEFAULT 0,                                      /* Number of Rows */
  Type_Htype INTEGER DEFAULT 0,                                        /* The shape of the heliostat */
    Round_Htype BOOLEAN DEFAULT false,                                 /* The heliostat is round */
    Rectangular_Htype BOOLEAN DEFAULT false,                           /* The heliostat is rectangular */
    Pentagon_Htype BOOLEAN DEFAULT false,                              /* The heliostat shape is a pentagon */
  RatioGlass_Htype DOUBLE PRECISION DEFAULT 0.0,                       /* Ratio of reflective glass to total heliostat surface area (Ratio) */
  Reflectivity_Htype DOUBLE PRECISION DEFAULT 0.0,                     /* Reflectivity of heliostat newly installed (Ratio) */
  dReflectivityPerYear_Htype DOUBLE PRECISION DEFAULT 0.0,             /* The degradation of reflectivity of heliostat per year (Ratio) */
  DateCanting_Htype TIMESTAMPTZ DEFAULT now(),                         /* Date Reference for Canting. Canting Focusing is as specied at this moment. */
  WindSpeedMaximum_Htype DOUBLE PRECISION DEFAULT 0.0,                 /* The maximum speed that this type of heliostat can withstand before stowing (m/s) */
  AngleHighAzimuthal_Htype DOUBLE PRECISION DEFAULT 0.0,               /* The upper angle azimuthal movement limit (degrees) */
  AngleLowAzimuthal_Htype DOUBLE PRECISION DEFAULT 0.0,                /* The lower angle azimuthal movement limit (degrees) */
  AngleHighElevation_Htype DOUBLE PRECISION DEFAULT 0.0,               /* The upper angle elevation movement limit (degrees) */
  AngleLowElevation_Htype DOUBLE PRECISION DEFAULT 0.0,                /* The lower angle elevation movement limit (degrees) */
  AngleStowAzimuthal_Htype DOUBLE PRECISION DEFAULT 0.0,               /* The stow azimuthal angle for this type of heliostat (degrees) */
  AngleStowElevation_Htype DOUBLE PRECISION DEFAULT 0.0,               /* The stow elevationangle for this type of heliostat (degrees) */
  AngleWashAzimuthal_Htype DOUBLE PRECISION DEFAULT 0.0,               /* The wash azimuthal angle for this type of heliostat (degrees) */
  AngleWashElevation_Htype DOUBLE PRECISION DEFAULT 0.0,               /* The wash elevation angle for this type of heliostat (degrees) */
  AngleRainAzimuthal_Htype DOUBLE PRECISION DEFAULT 0.0,               /* The rainwash azimuthal angle for this type of heliostat (degrees) */
  AngleRainElevation_Htype DOUBLE PRECISION DEFAULT 0.0,               /* The rainwash elevation angle for this type of heliostat (degrees) */
  MaxAzCountsforFastRate_Htype INTEGER DEFAULT 0,                      /* (Not used) */
  MaxAzCountsforSlowRate_Htype INTEGER DEFAULT 0,                      /* (Not used) */
  MaxElCountsforFastRate_Htype INTEGER DEFAULT 0,                      /* (Not used) */
  MaxElCountsforSlowRate_Htype INTEGER DEFAULT 0,                      /* (Not used) */
  CountMaxFastAzimuthal_Htype INTEGER DEFAULT 0,                       /* The reduced azimuthal encoder speed in fast rate when close to the target (counts/minute) */
  CountMaxSlowAzimuthal_Htype INTEGER DEFAULT 0,                       /* The reduced azimuthal encoder speed in slow rate when close to the target (counts/minute) */
  CountMaxFastElevation_Htype INTEGER DEFAULT 0,                       /* The reduced elevation encoder speed in fast rate when close to the target (counts/minute) */
  CountMaxSlowElevation_Htype INTEGER DEFAULT 0,                       /* The reduced elevation encoder speed in slow rate when close to the target (counts/minute) */
  DerateMaxCountAzimuthal_Htype DOUBLE PRECISION DEFAULT 0.0,          /* Derating multiplier for the azimuthal slow rate (fraction) */
  DerateMaxCountElevation_Htype DOUBLE PRECISION DEFAULT 0.0,          /* Derating multiplier for the elevation slow rate (fraction) */
  AzFastRate_Htype DOUBLE PRECISION DEFAULT 0.0,                       /* The azimuthal nominal high speed rate of this type of heliostat (degrees/minute) */
  AzSlowRate_Htype DOUBLE PRECISION DEFAULT 0.0,                       /* The azimuthal nominal slow speed rate of this type of heliostat (degrees/minute) */
  ElFastRate_Htype DOUBLE PRECISION DEFAULT 0.0,                       /* The elevation nominal high speed rate of this type of heliostat (degrees/minute) */
  ElSlowRate_Htype DOUBLE PRECISION DEFAULT 0.0,                       /* The elevation nominal slow speed rate of this type of heliostat (degrees/minute) */
  dAngleFastAzimuthal_Htype DOUBLE PRECISION DEFAULT 0.0,              /* The azimuthal maximum high speed rate of this type of heliostat (degrees/minute) */
  dAngleSlowAzimuthal_Htype DOUBLE PRECISION DEFAULT 0.0,              /* The azimuthal maximum slow speed rate of this type of heliostat (degrees/minute) */
  dAngleFastElevation_Htype DOUBLE PRECISION DEFAULT 0.0,              /* The elevation maximum high speed rate of this type of heliostat (degrees/minute) */
  dAngleSlowElevation_Htype DOUBLE PRECISION DEFAULT 0.0,              /* The elevation maximum slow speed rate of this type of heliostat (degrees/minute) */
  XMirrorOffset_Htype DOUBLE PRECISION DEFAULT 0.0,                    /* The horizontal offset in heliostat local coordinates from the mirror center to the pivot (m) */
  YMirrorOffset_Htype DOUBLE PRECISION DEFAULT 0.0,                    /* The normal offset in heliostat local coordinates from the mirror center to the pivot (m) */
  ZMirrorOffset_Htype DOUBLE PRECISION DEFAULT 0.0,                    /* The vertical offset in heliostat local coordinates from the mirror center to the pivot (m) */
  ZPivot_Htype DOUBLE PRECISION DEFAULT 0.0,                           /* The height of the heliostat pivot point from the concrete base to which the post is attaches (m) */
  dIdXTarget_Htype DOUBLE PRECISION DEFAULT 0.0,                       /* Display conversion factor for East/West direction (Pixels per meter) */
  dJdZTarget_Htype DOUBLE PRECISION DEFAULT 0.0,                       /* Display conversion factor for Up/Down direction (Pixels per meter) */
  GainAzimuthalNominal_Htype DOUBLE PRECISION DEFAULT 0.0,             /* The nominal multiplier for converting azimuthal degrees to counts (Counts per Degree) */
  GainElevationNominal_Htype DOUBLE PRECISION DEFAULT 0.0,             /* The nominal multiplier for converting elevation degrees to counts (Counts per Degree) */
  MinutesForInitializingEl_Htype INTEGER DEFAULT 0,                    /* Time out in minutes for initializing the EL drive */
  MinutesForInitializingAz_Htype INTEGER DEFAULT 0,                    /* Time out in minutes for initializing the EL drive */
  SigmaAzAiming_Htype DOUBLE PRECISION DEFAULT 0.0,                    /* Standard deviation of nominal azimuthal aiming error (radians) */
  SigmaElAiming_Htype DOUBLE PRECISION DEFAULT 0.0,                    /* Standard deviation of nominal elevation aiming error (radians) */
  SigmaSlopeX_Htype DOUBLE PRECISION DEFAULT 0.0,                      /* Standard deviation of nominal slope error along the horizontal direction (radians) */
  SigmaSlopeY_Htype DOUBLE PRECISION DEFAULT 0.0,                      /* Standard deviation of nominal slope error along the vertical direction (radians) */
  ReflectivityMeasured_Htype DOUBLE PRECISION DEFAULT 0.0,             /* Average field reflectivity as measured (percent) */
  BalanceFactor_Htype DOUBLE PRECISION DEFAULT 0.0,                    /* Used while reconciling reflectivity spillage etc for the Receiver Energy Balance (fraction) */
  P$Cant_Htype INTEGER DEFAULT 0                                       /* Pointer to the canting groups for this type of heliostat */
);
  
/* Heliostat Canting Group */
CREATE TABLE Cant(
  Instance_Cant INTEGER PRIMARY KEY,                                   /* Primary Key for the Cant Table */
  ID_Cant VARCHAR(32) DEFAULT '',                                      /* ID of a canting group */
  RadiusMin_Cant DOUBLE PRECISION DEFAULT 0.0,                         /* Minimum distance from the tower center for heliostats with this cant (m) */
  RadiusMax_Cant DOUBLE PRECISION DEFAULT 0.0,                         /* Maximum distance from the tower center for heliostats with this cant (m) */
  FocalLengthWidth_Cant DOUBLE PRECISION DEFAULT 0.0,                  /* Distance where a normal ray reflection crosses the HZ Plane of the heliostat (m) */
  FocalLengthHeight_Cant DOUBLE PRECISION DEFAULT 0.0,                 /* Distance where a normal ray reflection crosses the ZW Plane of the heliostat (m) */
  P$Facet_Cant INTEGER DEFAULT 0                                       /* Pointer to the facet orientations for a canting group */
);
  
/* Heliostat Facet Canting Placement */
CREATE TABLE Facet(
  Instance_Facet INTEGER PRIMARY KEY,                                  /* Primary Key for the Facet Table */
  ID_Facet VARCHAR(32) DEFAULT '',                                     /* ID of a facet type */
  RadiusCurvatureWidth_Facet DOUBLE PRECISION DEFAULT 0.0,             /* Radius of curvature along the facet width (m) */
  RadiusCurvatureHeight_Facet DOUBLE PRECISION DEFAULT 0.0,            /* Radius of curvature along the facet height (m) */
  Column_Facet INTEGER DEFAULT 0,                                      /* Column assignment of this facet */
  Row_Facet INTEGER DEFAULT 0,                                         /* Row assignment of this facet */
  GapHorizontal_Facet DOUBLE PRECISION DEFAULT 0.0,                    /* Gap to facet in next column (m) */
  GapVertical_Facet DOUBLE PRECISION DEFAULT 0.0,                      /* Gap to facet in next row (m) */
  Xcenter_Facet DOUBLE PRECISION DEFAULT 0.0,                          /* Horizontal distance in heliostat local coordinates from the facet to the mirror center (m) */
  Ycenter_Facet DOUBLE PRECISION DEFAULT 0.0,                          /* Normal distance in heliostat local coordinates from the facet to the mirror center (m) */
  Zcenter_Facet DOUBLE PRECISION DEFAULT 0.0,                          /* Vertical distance in heliostat local coordinates from the facet to the mirror center (m) */
  AngleRotationAxisHeight_Facet DOUBLE PRECISION DEFAULT 0.0,          /* Rotation around the vertical axis to orient the facet to canted focal length (degrees) */
  AngleRotationAxisWidth_Facet DOUBLE PRECISION DEFAULT 0.0            /* Rotation around the horizontal axis to orient the facet to canted focal length (degrees) */
);
  
/* A place on the earth */
CREATE TABLE Place(
  Instance_Place INTEGER PRIMARY KEY,                                  /* Primary Key for the Place Table */
  ID_Place VARCHAR(32) DEFAULT '',                                     /* The Name of a place */
  Latitude_Place DOUBLE PRECISION DEFAULT 0.0,                         /* Latitude (WGS84) (Decimal Degrees) */
  Longitude_Place DOUBLE PRECISION DEFAULT 0.0,                        /* Longitude (WGS84)(Decimal Degrees) */
  ElevationGeodetic_Place DOUBLE PRECISION DEFAULT 0.0,                /* Elevation above the WGS84 Ellipsoid (m) */
  Elevation_Place DOUBLE PRECISION DEFAULT 0.0,                        /* Elevation above mean sea level (m) */
  VerticalDatum_Place VARCHAR(20) DEFAULT '',                          /* The name of the vertical datum for Elevation */
  NameTimeZone_Place VARCHAR(48) DEFAULT '',                           /* The Continent and City of this place */
  HoursGMT_Place DOUBLE PRECISION DEFAULT 0.0,                         /* The Hours Offset from GMT for standard time */
  HoursDST_Place DOUBLE PRECISION DEFAULT 0.0,                         /* The Hours Offset from GMT for daylight savings time */
  StringGMT_Place VARCHAR(8) DEFAULT '',                               /* The time String for standard time */
  StringDST_Place VARCHAR(8) DEFAULT '',                               /* The time String for daylight savings time */
  ZoneGeographicCoordinates_Place VARCHAR(8) DEFAULT '',               /* The Geographic Coordinates Zone (eg UTM-Zone31 or SPC-NV C) */
  TimeStampSunriseLocal_Place VARCHAR(24) DEFAULT '',                  /* Time stamp of sunrise in the local timezone */
  TimeStampSunsetLocal_Place VARCHAR(24) DEFAULT '',                   /* Time stamp of sunset in the local timezone */
  TimeStampSolarNoonLocal_Place VARCHAR(24) DEFAULT '',                /* Time stamp of solar noon in the local timezone */
  TimeStampSolarTransLocal_Place VARCHAR(24) DEFAULT '',               /* Time stamp of solar transit in the local timezone */
  TimeStampNowLocal_Place VARCHAR(24) DEFAULT '',                      /* Time stamp currently in the local timee zone */
  TimeStampSunriseUTC_Place VARCHAR(24) DEFAULT '',                    /* UTC stamp of sunrise */
  TimeStampSunsetUTC_Place VARCHAR(24) DEFAULT '',                     /* UTC stamp of sunset */
  TimeStampSolarNoonUTC_Place VARCHAR(24) DEFAULT '',                  /* UTC stamp of solar noon */
  TimeStampSolarTransUTC_Place VARCHAR(24) DEFAULT '',                 /* UTC stamp of solar transit */
  TimeStampNowUTC_Place VARCHAR(24) DEFAULT '',                        /* UTC stamp currently in the local timee zone */
  DegreesLatitude_Place INTEGER DEFAULT 0,                             /* WGS84 Degrees Latitude of the place (Degrees) */
  MinutesLatitude_Place INTEGER DEFAULT 0,                             /* WGS84 Minutes Latitude of the place (Arc-Minutes) */
  SecondsLatitude_Place DOUBLE PRECISION DEFAULT 0.0,                  /* WGS 84 Seconds Latitude of the place (Arc-Seconds) */
  DegreesLongitude_Place INTEGER DEFAULT 0,                            /* WGS84 Degrees Longitude of the place (Degrees) */
  MinutesLongitude_Place INTEGER DEFAULT 0,                            /* WGS84 Minutes Longitude of the place (Arc-Minutes) */
  SecondsLongitude_Place DOUBLE PRECISION DEFAULT 0.0,                 /* WGS 84 Seconds Longitude of the place (Arc-Seconds) */
  Type_Place INTEGER DEFAULT 0,                                        /* Specifies the direction of latitude and longitude for this place */
    LongitudeWest_Place BOOLEAN DEFAULT false,                         /* Set true for West, false for East. */
    LatitudeSouth_Place BOOLEAN DEFAULT false,                         /* Set true for South, false for North. */
  AsciiLatitude_Place VARCHAR(24) DEFAULT '',                          /* The ASCII latitude of the place for diaplay */
  AsciiLongitude_Place VARCHAR(24) DEFAULT '',                         /* The ASCII longitude of the place for diaplay */
  AngleClocking_Place DOUBLE PRECISION DEFAULT 0.0,                    /* The angle difference of True North and Field Construction North (degrees) */
  TempFReference_Place DOUBLE PRECISION DEFAULT 0.0,                   /* The reference temperature for airmass data for a place (degF) */
  PsiaReference_Place DOUBLE PRECISION DEFAULT 0.0,                    /* The reference absolute pressure for airmass data for a place (psia) */
  StateForLossOfComms_Place INTEGER DEFAULT 0,                         /* Defines how the field should be commanded for loss of communications with the DCS */
    DefocusForLossOfComm_Place BOOLEAN DEFAULT false,                  /* The Field Defocus command should be issued for loss of communication with the DCS */
    StandbyForLossOfComm_Place BOOLEAN DEFAULT false,                  /* The Field Standby command should be issued for loss of communication with the DCS */
  StowForLossPlantPower_Place INTEGER DEFAULT 0,                       /* The state to command the field to for loss plant power */
    StowDefocusLossOfPower_Place BOOLEAN DEFAULT false,                /* The Field Defocus command should be issued for Plant loss of power */
    StowWindForLossOfPower_Place BOOLEAN DEFAULT false,                /* The Field Wind Stow command should be issued for Plant loss of power */
    StowNightForLossOfPower_Place BOOLEAN DEFAULT false,               /* The Field Night Stow command should be issued for Plant loss of power */
  MotorToMoveForDefocus_Place INTEGER DEFAULT 0,                       /* The motor(s) allow to move during emergency defocus */
    MoveAzDriveForDefocus_Place BOOLEAN DEFAULT false,                 /* The Az Drive motor can be moved during emergency defocus */
    MoveElDriveForDefocus_Place BOOLEAN DEFAULT false,                 /* The El Drive motor can be moved during emergency defocus */
  MotorSpeedReduction_Place INTEGER DEFAULT 0,                         /* Enforce slow motor movement when Power Source(s) are active */
    ReduceSpeedForBackupPower_Place BOOLEAN DEFAULT false,             /* Reduce motor speeds when running under backup power */
    ReduceSpeedForPlantPower_Place BOOLEAN DEFAULT false,              /* Reduce motor speeds when running under plant power */
  P$Horizon_Place INTEGER DEFAULT 0                                    /* The pointer to the horizon around a Place */
);
  
/* The horizon around a place */
CREATE TABLE Horizon(
  Instance_Horizon INTEGER PRIMARY KEY,                                /* Primary Key for the Horizon Table */
  X_Horizon DOUBLE PRECISION DEFAULT 0.0,                              /* The East coordinate of a horizon point (m) */
  Y_Horizon DOUBLE PRECISION DEFAULT 0.0,                              /* The North coordinate of a horizon point (m) */
  Z_Horizon DOUBLE PRECISION DEFAULT 0.0                               /* The Elevation of a horizon point (m) */
);
  
/* A heliostat wash truck */
CREATE TABLE Truck(
  Instance_Truck INTEGER PRIMARY KEY,                                  /* Primary Key for the Truck Table */
  ID_Truck VARCHAR(32) DEFAULT '',                                     /* The name of the Truck */
  Command_Truck INTEGER DEFAULT 0,                                     /* Commands to the Truck */
    ClockWise_Truck BOOLEAN DEFAULT false,                             /* Move in a clock wise direction */
    CounterClockWise_Truck BOOLEAN DEFAULT false,                      /* Move in a counter clock wise direction */
    RequestWash_Truck BOOLEAN DEFAULT false,                           /* Request to start the wash */
    PositioningTOWash_Truck BOOLEAN DEFAULT false,                     /* Move to wash position */
    Washing_Truck BOOLEAN DEFAULT false,                               /* Commence washing */
    WashingCompleted_Truck BOOLEAN DEFAULT false,                      /* Finish washing */
  Starting$Heliostat_Truck INTEGER DEFAULT 0,                          /* The index of Starting heliostat for washing */
  NumWashed_Truck INTEGER DEFAULT 0                                    /* The Number of heliostat to be washed */
);
  
/* An logical ethernet network of devices */
CREATE TABLE Ethernet(
  Instance_Ethernet INTEGER PRIMARY KEY,                               /* Primary Key for the Ethernet Table */
  NumHFCUnknownState_Ethernet INTEGER DEFAULT 0,                       /* Number of HFCs in an unknown state */
  NumHFCGoodState_Ethernet INTEGER DEFAULT 0,                          /* Number of HFCs in an good state */
  NumHFCBadState_Ethernet INTEGER DEFAULT 0,                           /* Number of HFCs in an bad state */
  ID_Ethernet VARCHAR(32) DEFAULT '',                                  /* The name of the ethernet communication bus */
  P$Field_Ethernet INTEGER DEFAULT 0,                                  /* Hierarchical pointer to the parent field of this ethernet bus */
  NumPageDocument_Ethernet INTEGER DEFAULT 0,                          /* The number of pages in the document for this ethernet bus */
  DateReleaseDocument_Ethernet VARCHAR(16) DEFAULT '',                 /* The release time/date string for the document */
  Type_Ethernet INTEGER DEFAULT 0,                                     /* The type of ehernet network */
    MultiModeFiber_Ethernet BOOLEAN DEFAULT false,                     /* Multimode fiber is used for the communication network */
    SingleModeFiber_Ethernet BOOLEAN DEFAULT false,                    /* Singlemode fiber is used for the communication network */
    Heliostat_Ethernet BOOLEAN DEFAULT false,                          /* Twisted pair cable or wireless used for the communication network */
    Video_Ethernet BOOLEAN DEFAULT false,                              /* This network serves cameras */
  P$HFC_Ethernet INTEGER DEFAULT 0                                     /* Pointer to the Heliostat Field Controllers on this ethernet bus */
);
  
/* A computer rack of devices in an ethernet network */
CREATE TABLE Rack(
  Instance_Rack INTEGER PRIMARY KEY,                                   /* Primary Key for the Rack Table */
  ID_Rack VARCHAR(32) DEFAULT '',                                      /* The name of the computer equipment rack */
  P$Mount_Rack INTEGER DEFAULT 0                                       /* The pointer to the equipment mounted in the rack */
);
  
/* An equipment mounted in a Rack */
CREATE TABLE Mount(
  Instance_Mount INTEGER PRIMARY KEY,                                  /* Primary Key for the Mount Table */
  ID_Mount VARCHAR(32) DEFAULT '',                                     /* The name of the computer equipment mounted in a rack */
  Type_Mount INTEGER DEFAULT 0,                                        /* The type of equipment mounted */
    Server_Mount BOOLEAN DEFAULT false,                                /* The equipment is a server. */
    SwitchNet_Mount BOOLEAN DEFAULT false,                             /* The equipment is a network switch */
  I$Server_Mount INTEGER DEFAULT 0,                                    /* The index of the server mounted in the rack */
  I$SwitchNet_Mount INTEGER DEFAULT 0                                  /* The index of the network switch mounted in the rack */
);
  
/* An ethernet network switch */
CREATE TABLE SwitchNet(
  Instance_SwitchNet INTEGER PRIMARY KEY,                              /* Primary Key for the SwitchNet Table */
  ID_SwitchNet VARCHAR(32) DEFAULT '',                                 /* The name of the network switch */
  P$PortNet_SwitchNet INTEGER DEFAULT 0                                /* The pointer to the ports on the switch */
);
  
/* A port on an ethernet network Switch */
CREATE TABLE PortNet(
  Instance_PortNet INTEGER PRIMARY KEY,                                /* Primary Key for the PortNet Table */
  ID_PortNet VARCHAR(32) DEFAULT '',                                   /* The ID of the network port. */
  Name_PortNet VARCHAR(8) DEFAULT '',                                  /* The name of the port */
  Conn$CableNet_PortNet INTEGER DEFAULT 0                              /* The network cable connected to the port */
);
  
/* Server records */
CREATE TABLE Server(
  Instance_Server INTEGER PRIMARY KEY,                                 /* Primary Key for the Server Table */
  ID_Server VARCHAR(32) DEFAULT '',                                    /* The ID/Name of the server */
  Type_Server INTEGER DEFAULT 0,                                       /* The type of server */
    Computer_Server BOOLEAN DEFAULT false,                             /* A computer (server) */
    Workstation_Server BOOLEAN DEFAULT false,                          /* A computer (workstation) */
    NTP_Server BOOLEAN DEFAULT false,                                  /* A time server */
    NAS_Server BOOLEAN DEFAULT false,                                  /* A network storage server */
  P$NIC_Server INTEGER DEFAULT 0,                                      /* The pointer to the network interface connections on the server */
  P$Drive_Server INTEGER DEFAULT 0,                                    /* The pointer to the disks on the server */
  C$Drive_Server INTEGER DEFAULT 0,                                    /* The pointer to the physical C drive of the server */
  D$Drive_Server INTEGER DEFAULT 0,                                    /* The pointer to the physical D drive of the server */
  F$Drive_Server INTEGER DEFAULT 0,                                    /* The pointer to the physical F drive of the server */
  I1$NIC_Server INTEGER DEFAULT 0,                                     /* The pointer to the 1st NIC on the server */
  I2$NIC_Server INTEGER DEFAULT 0,                                     /* The pointer to the 2nd NIC on the server */
  I3$NIC_Server INTEGER DEFAULT 0,                                     /* The pointer to the 3rd NIC on the server */
  I4$NIC_Server INTEGER DEFAULT 0,                                     /* The pointer to the 4th NIC on the server */
  I5$NIC_Server INTEGER DEFAULT 0,                                     /* The pointer to the 5th NIC on the server */
  I6$NIC_Server INTEGER DEFAULT 0,                                     /* The pointer to the 6th NIC on the server */
  I7$NIC_Server INTEGER DEFAULT 0,                                     /* The pointer to the 7th NIC on the server */
  I8$NIC_Server INTEGER DEFAULT 0                                      /* The pointer to the 8th NIC on the server */
);
  
/* Disk drive records */
CREATE TABLE Drive(
  Instance_Drive INTEGER PRIMARY KEY,                                  /* Primary Key for the Drive Table */
  ID_Drive VARCHAR(32) DEFAULT '',                                     /* The disk on the server */
  Descrip_Drive VARCHAR(32) DEFAULT '',                                /* The description of the disk drive */
  LetterAssigned_Drive VARCHAR(2) DEFAULT '',                          /* The drive letter assignment to this drive */
  NameShare_Drive VARCHAR(32) DEFAULT '',                              /* The share name of this drive */
  NetworkLocation_Drive VARCHAR(32) DEFAULT '',                        /* The location on the network of this drive */
  NetUseStatement_Drive VARCHAR(32) DEFAULT '',                        /* The Net Use statement to mount this drive */
  Status_Drive INTEGER DEFAULT 0,                                      /* The disk drive availability status */
    Availab_Drive BOOLEAN DEFAULT false,                               /* The disk drive is available */
    Unavail_Drive BOOLEAN DEFAULT false,                               /* The disk drive is not available */
    Unknown_Drive BOOLEAN DEFAULT false,                               /* The disk drive availability cannot be determined */
  SStatus_Drive INTEGER DEFAULT 0,                                     /* The disk drive free space status */
    SAvail_Drive BOOLEAN DEFAULT false,                                /* The disk drive free space data is available */
    Sunavail_Drive BOOLEAN DEFAULT false,                              /* The disk drive free space data is not available */
    Sunknown_Drive BOOLEAN DEFAULT false,                              /* The disk drive free space data availability cannot be determined */
    SNormal_Drive BOOLEAN DEFAULT false,                               /* The is ample free space on this drive */
    SWarning_Drive BOOLEAN DEFAULT false,                              /* The free space on this drive has fallen below 15% */
    SFatal_Drive BOOLEAN DEFAULT false,                                /* The free space on this drive has fallen below 5% */
  WStatus_Drive INTEGER DEFAULT 0,                                     /* The disk drive write status */
    WUnknown_Drive BOOLEAN DEFAULT false,                              /* The disk drive write status cannot be determined */
    Wavail_Drive BOOLEAN DEFAULT false,                                /* Data can be written to this disk drive */
    WUnavail_Drive BOOLEAN DEFAULT false,                              /* Data cannot be written to this disk drive */
  Dataq_Drive INTEGER DEFAULT 0,                                       /* The disk drive status data quality */
    UpToDate_Drive BOOLEAN DEFAULT false,                              /* The disk drive status data is up to date */
    OutofDat_Drive BOOLEAN DEFAULT false,                              /* The disk drive status data is stale */
  Inhibit_Drive INTEGER DEFAULT 0,                                     /* Inhibit status from Disk Drive reporting */
    BlkEd_Drive BOOLEAN DEFAULT false,                                 /* Disk Drive data reporting has been blocked */
    UnBlked_Drive BOOLEAN DEFAULT false,                               /* Disk Drive data reporting has been unblocked */
  PercentAvailable_Drive DOUBLE PRECISION DEFAULT 0.0,                 /* The Percent of available space on this disk on this drive(percent) */
  PercentUsedSpace_Drive DOUBLE PRECISION DEFAULT 0.0,                 /* The available used space on this disk on this drive (percent) */
  CapacityMegabytes_Drive DOUBLE PRECISION DEFAULT 0.0                 /* The megabyte storage capacity of this drive */
);
  
/* Network Interface Connection in a server */
CREATE TABLE NIC(
  Instance_NIC INTEGER PRIMARY KEY,                                    /* Primary Key for the NIC Table */
  ID_NIC VARCHAR(32) DEFAULT '',                                       /* The ID of the NIC */
  Name_NIC VARCHAR(4) DEFAULT '',                                      /* The name of the NIC */
  Type_NIC INTEGER DEFAULT 0,                                          /* The type of NIC */
    Team_NIC BOOLEAN DEFAULT false,                                    /* This NIC is a logical team. */
    TeamMember_NIC BOOLEAN DEFAULT false,                              /* This NIC is a physical member of a team */
    StandAlone_NIC BOOLEAN DEFAULT false,                              /* This NIC is not a member of a team */
  Status_NIC INTEGER DEFAULT 0,                                        /* The availability status of the NIC */
    Availab_NIC BOOLEAN DEFAULT false,                                 /* The NIC is available */
    Unavail_NIC BOOLEAN DEFAULT false,                                 /* The NIC is not available */
    Unknown_NIC BOOLEAN DEFAULT false,                                 /* The NIC availability is unknown */
  NetConnectionStatus_NIC INTEGER DEFAULT 0,                           /* Network connection status from WMIC */
    Disconnected_NIC BOOLEAN DEFAULT false,                            /* NIC is Disconnected */
    Connecting_NIC BOOLEAN DEFAULT false,                              /* NIC is connecting */
    Connected_NIC BOOLEAN DEFAULT false,                               /* NIC is Connecting */
    Disconnecting_NIC BOOLEAN DEFAULT false,                           /* NIC is Disconnecting */
    HardwareNotPresent_NIC BOOLEAN DEFAULT false,                      /* NIC attached hardware is not present */
    HardwareDisabled_NIC BOOLEAN DEFAULT false,                        /* NIC attached hardware is not disabled */
    HardwareMalfunction_NIC BOOLEAN DEFAULT false,                     /* NIC attached hardware has malfunctioned */
    MediaDisconnected_NIC BOOLEAN DEFAULT false,                       /* NIC ethernet plug is not connected */
    Authenticating_NIC BOOLEAN DEFAULT false,                          /* NIC is authenticating */
    AuthenticationSucceeded_NIC BOOLEAN DEFAULT false,                 /* NIC authentication has succeded */
    AuthenticationFailed_NIC BOOLEAN DEFAULT false,                    /* NIC authentication has failed */
    InvalidAddress_NIC BOOLEAN DEFAULT false,                          /* NIC IP Address is invalid */
    CredentialsRequired_NIC BOOLEAN DEFAULT false,                     /* Credentials are needed to connect */
  Dataq_NIC INTEGER DEFAULT 0,                                         /* NIC data quality */
    UpToDate_NIC BOOLEAN DEFAULT false,                                /* NIC data is current */
    OutofDat_NIC BOOLEAN DEFAULT false,                                /* NIC data is stale */
  Inhibit_NIC INTEGER DEFAULT 0,                                       /* Inhibit Status for NIC reporting */
    BlkEd_NIC BOOLEAN DEFAULT false,                                   /* NIC data reporting has been blocked */
    UnBlked_NIC BOOLEAN DEFAULT false,                                 /* NIC data reporting has been unblocked */
  Conn$CableNet_NIC INTEGER DEFAULT 0,                                 /* The network cable plugged into the NIC */
  Description_NIC VARCHAR(64) DEFAULT ''                               /* The description of the NIC */
);
  
/* A network connection cable */
CREATE TABLE CableNet(
  Instance_CableNet INTEGER PRIMARY KEY,                               /* Primary Key for the CableNet Table */
  ID_CableNet VARCHAR(32) DEFAULT '',                                  /* The name of the network cable */
  Type_CableNet INTEGER DEFAULT 0,                                     /* The type of network cable connection */
    ServerToSwitch_CableNet BOOLEAN DEFAULT false,                     /* This network cable connects a server to a network switch */
    ServerToServer_CableNet BOOLEAN DEFAULT false,                     /* This network cable connects two servers */
    SwitchToSwitch_CableNet BOOLEAN DEFAULT false,                     /* This network cable connects two switches */
  From$NIC_CableNet INTEGER DEFAULT 0,                                 /* The "From" NIC of the network cable. */
  From$PortNet_CableNet INTEGER DEFAULT 0,                             /* The "From" Switch Port of the network cable */
  To$PortNet_CableNet INTEGER DEFAULT 0,                               /* The "To" Switch Port of the network cable. */
  To$NIC_CableNet INTEGER DEFAULT 0,                                   /* The "From" NIC of the network cable. */
  Description_CableNet VARCHAR(64) DEFAULT ''                          /* The network cable description */
);
  
/* A Heliostat Field Controller */
CREATE TABLE HFC(
  Instance_HFC INTEGER PRIMARY KEY,                                    /* Primary Key for the HFC Table */
  NumHeliostatsTrack_HFC INTEGER DEFAULT 0,                            /* Number of heliostats in the tracking mode */
  NumHeliostatsTracking_HFC INTEGER DEFAULT 0,                         /* Number of heliostats tracking */
  NumHeliostatsStow_HFC INTEGER DEFAULT 0,                             /* Number of heliostats in emergency stow mode */
  NumHeliostatsStowed_HFC INTEGER DEFAULT 0,                           /* Number of heliostats at emergency stow */
  NumHeliostatsDefocus_HFC INTEGER DEFAULT 0,                          /* Number of heliostats in emergency defocus mode */
  NumHeliostatsDefocused_HFC INTEGER DEFAULT 0,                        /* Number of heliostats at emergency defocus */
  NumHeliostatsSleep_HFC INTEGER DEFAULT 0,                            /* Number of heliostats in sleep mode */
  NumHeliostatsSleeping_HFC INTEGER DEFAULT 0,                         /* Number of heliostats asleep */
  NumHeliostatsAtRainWash_HFC INTEGER DEFAULT 0,                       /* Number of heliostats at rain wash position */
  NumHeliostatsRainWash_HFC INTEGER DEFAULT 0,                         /* Number of heliostats in wash mode */
  NumHeliostatsWash_HFC INTEGER DEFAULT 0,                             /* Number of heliostats in wash mode */
  NumHeliostatsAtWash_HFC INTEGER DEFAULT 0,                           /* Number of heliostats at wash position */
  NumHeliostatsManual_HFC INTEGER DEFAULT 0,                           /* Number of heliostats in manual position mode */
  NumHeliostatsAtManual_HFC INTEGER DEFAULT 0,                         /* Number of heliostats at the manually position */
  NumHeliostatsStop_HFC INTEGER DEFAULT 0,                             /* Number of heliostats in emergency stop mode */
  NumHeliostatsStopped_HFC INTEGER DEFAULT 0,                          /* Number of heliostats succefully stopped for emergency purposes */
  NumHeliostatsStandby_HFC INTEGER DEFAULT 0,                          /* Number of heliostats in standby mode */
  NumHeliostatsAtStandby_HFC INTEGER DEFAULT 0,                        /* Number of heliostats at the standby position */
  NumHeliostatsBCS_HFC INTEGER DEFAULT 0,                              /* Number of heliostats being processed by the BCS */
  NumHeliostatsAtBCS_HFC INTEGER DEFAULT 0,                            /* Number of heliostats being imaged by the BCS */
  NumHeliostatsGoodEncoders_HFC INTEGER DEFAULT 0,                     /* Number of heliostats with encoder confidence */
  NumHeliostatsSeekingMark_HFC INTEGER DEFAULT 0,                      /* Number of heliostats recalibrating encoder */
  NumHeliostatsOffLine_HFC INTEGER DEFAULT 0,                          /* Number of heliostats in the off-line state */
  NumHeliostatsError_HFC INTEGER DEFAULT 0,                            /* Number of heliostats in an error state */
  NumHeliostatsAnyWash_HFC INTEGER DEFAULT 0,                          /* Number of heliostats in either rain wash or ordinary wash mode */
  NumHeliostatsAtAnyWash_HFC INTEGER DEFAULT 0,                        /* Number of heliostats at either rain wash or ordinary wash position */
  StatusSet_HFC INTEGER DEFAULT 0,                                     /* The UI operator can command all selected heliostats in an HFC. */
    SelectWash_HFC BOOLEAN DEFAULT false,                              /* Select that selected heliostats within this HFC be put into wash position. */
    SelectWashCancel_HFC BOOLEAN DEFAULT false,                        /* Return selected heliostats in wash position in this HFC to the field control */
    SelectWashComplete_HFC BOOLEAN DEFAULT false,                      /* Records selected heliostats wash complete */
    SelectRainPosition_HFC BOOLEAN DEFAULT false,                      /* Moves selected heliostats within this HFC into rain wash position. */
    SelectDefrost_HFC BOOLEAN DEFAULT false,                           /* Set selected heliostats within this HFC to Defrost. */
    SelectStandby_HFC BOOLEAN DEFAULT false,                           /* Moves selected heliostats within this HFC into standby position. */
    SelectStow_HFC BOOLEAN DEFAULT false,                              /* Moves selected heliostats within this HFC to wind stow position. */
    SelectSleep_HFC BOOLEAN DEFAULT false,                             /* Moves selected heliostats within this HFC to night stow position. */
    SelectOperator_HFC BOOLEAN DEFAULT false,                          /* Puts selected heliostats within this HFC into operator command. */
    SelectAutomatic_HFC BOOLEAN DEFAULT false,                         /* Puts selected heliostats within this HFC into automatic command. */
  CommandHeliostats_HFC INTEGER DEFAULT 0,                             /* The UI operator can command all the heliostats in the HFC as follows */
    InitializeAz_HFC BOOLEAN DEFAULT false,                            /* Initialize all the Az drives */
    InitializeEl_HFC BOOLEAN DEFAULT false,                            /* Initialize all the El drives */
    OfflineAz_HFC BOOLEAN DEFAULT false,                               /* Place all the Az drives offline */
    OfflineEl_HFC BOOLEAN DEFAULT false,                               /* Place all the Az drives offline */
    ResetAz_HFC BOOLEAN DEFAULT false,                                 /* Reset all the Az drives */
    ResetEl_HFC BOOLEAN DEFAULT false,                                 /* Reset all the El drives */
    MarkAz_HFC BOOLEAN DEFAULT false,                                  /* Command all Az drives to find their home position */
    MarkEl_HFC BOOLEAN DEFAULT false,                                  /* Command all El drives to find their home position */
  StartOperation$HCU_HFC INTEGER DEFAULT 0,                            /* The starting heliostat control unit that the operator has selected for this HFC (for common operations) */
  EndOperation$HCU_HFC INTEGER DEFAULT 0,                              /* The ending heliostat control unit that the operator has selected for this HFC (for common operations) */
  ID_HFC VARCHAR(32) DEFAULT '',                                       /* The name of the heliostat field controller */
  FirmwareRevision_HFC VARCHAR(24) DEFAULT '',                         /* The firmware version of the HFC */
  Latitude_HFC DOUBLE PRECISION DEFAULT 0.0,                           /* The surveyed WGS84 latitude of the Heliostat Field Controller foundation (degrees) */
  Longitude_HFC DOUBLE PRECISION DEFAULT 0.0,                          /* The surveyed WGS84 longitude of the Heliostat Field Controller foundation (degrees) */
  Elevation_HFC DOUBLE PRECISION DEFAULT 0.0,                          /* The surveyed WGS84 elevation of the Heliostat Field Controller foundation (m) */
  dLatitude_HFC DOUBLE PRECISION DEFAULT 0.0,                          /* The surveyed Northing coordinate of the HFC foundation (us survey feet) */
  dLongitude_HFC DOUBLE PRECISION DEFAULT 0.0,                         /* The surveyed Easting coordinate of the HFC foundation (us survey feet) */
  XLocation_HFC DOUBLE PRECISION DEFAULT 0.0,                          /* The ENU East coordinate of the Heliostat Field Controller foundation (m) */
  YLocation_HFC DOUBLE PRECISION DEFAULT 0.0,                          /* The ENU North coordinate of the Heliostat Field Controller foundation (m) */
  ZLocation_HFC DOUBLE PRECISION DEFAULT 0.0,                          /* The ENU Up coordinate of the Heliostat Field Controller foundation (m) */
  Address_HFC VARCHAR(32) DEFAULT '',                                  /* The TCP/IP Address of the HFC */
  PageInDocument_HFC INTEGER DEFAULT 0,                                /* The page number in the document for this HFC */
  P$Ethernet_HFC INTEGER DEFAULT 0,                                    /* Pointer to the parent ethernet bus of the heliostat field controller */
  EthernetA$HFC_HFC INTEGER DEFAULT 0,                                 /* Pointer to the HFC on the A ethernet card of the HFC */
  EthernetB$HFC_HFC INTEGER DEFAULT 0,                                 /* Pointer to the HFC on the B ethernet card of the HFC */
  Type_HFC INTEGER DEFAULT 0,                                          /* The type of Heliostat Field Controller */
    Heliostat_HFC BOOLEAN DEFAULT false,                               /* The controller is a Heliostat Field Control Unit */
    Switch_HFC BOOLEAN DEFAULT false,                                  /* This controller is a network switch */
    Server_HFC BOOLEAN DEFAULT false,                                  /* The controller is a Server */
    Video_HFC BOOLEAN DEFAULT false,                                   /* The controller is a camera */
    WeatherMeter_HFC BOOLEAN DEFAULT false,                            /* The controller is a weather station */
    NoIPAddress_HFC BOOLEAN DEFAULT false,                             /* The controller does not have an IP address */
  A$Serial_HFC INTEGER DEFAULT 0,                                      /* Pointer to the Serial Bus connected to the A card of the HFC */
  B$Serial_HFC INTEGER DEFAULT 0,                                      /* Pointer to the Serial Bus connected to the B card of the HFC */
  C$Serial_HFC INTEGER DEFAULT 0,                                      /* Pointer to the Serial Bus connected to the C card of the HFC */
  D$Serial_HFC INTEGER DEFAULT 0,                                      /* Pointer to the Serial Bus connected to the D card of the HFC */
  IDEquip_HFC VARCHAR(12) DEFAULT '',                                  /* The configuration control equipment ID of the HFC */
  Equip_HFC INTEGER DEFAULT 0,                                         /* The configuration control equipment index of the HFC */
  Ctrl$Field_HFC INTEGER DEFAULT 0,                                    /* Pointer to the field controlled by this HFC (if it is a server) */
  Ctrl$Receiver_HFC INTEGER DEFAULT 0,                                 /* Pointer to the receiver controlled by this HFC (if it is a server) */
  P$Serial_HFC INTEGER DEFAULT 0,                                      /* Pointer the Serial Communication buses for the HFC */
  P$Video_HFC INTEGER DEFAULT 0,                                       /* Pointer to Video IP Connections */
  P$Switch_HFC INTEGER DEFAULT 0,                                      /* Pointer to Ethernet Switches for this HFC */
  CommStatus_HFC INTEGER DEFAULT 0,                                    /* Communication status of the HFC */
    CommunicationsGood_HFC BOOLEAN DEFAULT false,                      /* Communications are up */
    CommunicationsUnknown_HFC BOOLEAN DEFAULT false,                   /* Communications status cannot be determined */
    CommunicationsBad_HFC BOOLEAN DEFAULT false,                       /* Communications are down */
    UnacknowledgedFailure_HFC BOOLEAN DEFAULT false,                   /* There has been a unacknowledged loss of heartbead */
    Invalid_HFC BOOLEAN DEFAULT false,                                 /* This HFC is not configured to run in the system */
  P$Analog_HFC INTEGER DEFAULT 0,                                      /* Pointer to the child analogs for each HFC */
  P$Setpnt_HFC INTEGER DEFAULT 0,                                      /* Pointer to the child setpnts for each HFC */
  P$TagName_HFC INTEGER DEFAULT 0,                                     /* Pointer to the start of the tags for each HFC */
  IOneLine_HFC INTEGER DEFAULT 0,                                      /* The lateral coordinate of the HFC in the oneline display (pixels) */
  JOneLine_HFC INTEGER DEFAULT 0,                                      /* The vertical coordinate of the HFC in the oneline display (pixels) */
  IMapBoard_HFC INTEGER DEFAULT 0,                                     /* The lateral coordinate of the HFC in the map board (pixels) */
  JMapBoard_HFC INTEGER DEFAULT 0,                                     /* The lateral coordinate of the HFC in the map board (pixels) */
  ScaleEthernetOneLine_HFC DOUBLE PRECISION DEFAULT 0.0,               /* Scaling to automatically place the Ethernet bus on the oneline display (future) */
  AngleEthernetOneLine_HFC INTEGER DEFAULT 0,                          /* Angle to automatically place the Ethernet  on the oneline display (future) */
  OrderEthernetWiring_HFC INTEGER DEFAULT 0,                           /* The wiring order of the HFC in the ethernet bus (If the EPC has to modify the order) */
  SourcePDU$Power_HFC INTEGER DEFAULT 0,                               /* The High Voltage Source for the HFC and its heliostats */
  SourceUPS$Power_HFC INTEGER DEFAULT 0,                               /* The UPS Source supplying control power for the HFC */
  PDU$Bus_HFC INTEGER DEFAULT 0,                                       /* The High Voltage Bus supplying power to the HFC and its heliostats */
  UPS$Bus_HFC INTEGER DEFAULT 0,                                       /* The UPS Bus supplying control power to HFC */
  PDU$Connect_HFC INTEGER DEFAULT 0,                                   /* The breaker/switch connecting the HFC to the High Voltage Power Source */
  UPS$Connect_HFC INTEGER DEFAULT 0,                                   /* The brealer/switch connecting the HFC to the UPS control power supply */
  IDBusPowerSourcePDU_HFC VARCHAR(20) DEFAULT '',                      /* The ID of the High Voltage Power Supply Bus for the HFC */
  IDBusPowerSourceUPS_HFC VARCHAR(20) DEFAULT '',                      /* The ID of the UPS Control Power Supply Bus for the HFC */
  IDBusSourcePDU_HFC VARCHAR(20) DEFAULT '',                           /* The ID of the High Voltage Power Supply Bus for the HFC */
  IDBusPDU_HFC VARCHAR(20) DEFAULT '',                                 /* The EPC Name of the High Voltage Power Supply Bus for the HFC */
  IDCableSourcePDU_HFC VARCHAR(20) DEFAULT '',                         /* The ID of the Power Supply Cable from the High Voltage Power Bus to the HFC */
  IDCableSourceCBPDU_HFC VARCHAR(20) DEFAULT '',                       /* The ID of the Circuit Breaker from the High Voltage Power Bus to the HFC */
  SourcePDU$HFC_HFC INTEGER DEFAULT 0,                                 /* The upstream HFC through which High Voltage Power is routed to the HFC */
  DownstreamPDU$HFC_HFC INTEGER DEFAULT 0,                             /* The downstream HFC receiving High Voltage Power routed through this HFC */
  DrawingLocation_HFC VARCHAR(20) DEFAULT '',                          /* The location of the HFC on the drawing of the Power System */
  IDBusSourceUPS_HFC VARCHAR(20) DEFAULT '',                           /* The ID of the UPS Control Power Source Bus for the HFC */
  IDBusUPS_HFC VARCHAR(20) DEFAULT '',                                 /* The EPC name of the UPS Control Power Bus for the HFC */
  IDCableSourceUPS_HFC VARCHAR(20) DEFAULT '',                         /* The ID of the Power Supply Cable from the Control Power Bus to the HFC */
  IDCableSourceCBUPS_HFC VARCHAR(20) DEFAULT '',                       /* The ID of the Circuit Breaker from the Control Power Bus to the HFC */
  SourceUPS$HFC_HFC INTEGER DEFAULT 0,                                 /* The upstream HFC through which Control  Power is routed to the HFC */
  DownstreamUPS$HFC_HFC INTEGER DEFAULT 0,                             /* The downstream HFC receiving Control Power routed through this HFC */
  IDLocalCBUPS_HFC VARCHAR(20) DEFAULT '',                             /* The EPC name of the Circuit Breaker from the Control Power Bus to the HFC */
  P$PwrLink_HFC INTEGER DEFAULT 0                                      /* Pointer to the power source linkages for an HFC */
);
  
/* An HFC connection switch */
CREATE TABLE Switch(
  Instance_Switch INTEGER PRIMARY KEY,                                 /* Primary Key for the Switch Table */
  ID_Switch VARCHAR(32) DEFAULT '',                                    /* The name of the Switch */
  Equipment_Switch VARCHAR(16) DEFAULT '',                             /* The Equipment tag for this switch */
  Address_Switch VARCHAR(20) DEFAULT '',                               /* The IP Address for this switch */
  Connect$HFC_Switch INTEGER DEFAULT 0,                                /* The controller connected to this switch */
  Type_Switch INTEGER DEFAULT 0,                                       /* The type of switch */
    IPAddress_Switch BOOLEAN DEFAULT false                             /* The switch has an IP Address */
);
  
/* An HFC Serial Communication Bus */
CREATE TABLE Serial(
  Instance_Serial INTEGER PRIMARY KEY,                                 /* Primary Key for the Serial Table */
  StatusSet_Serial INTEGER DEFAULT 0,                                  /* The UI operator can set all heliostat in the Serial Bus to Automatic of Manual control mode. */
    HeliostatsAutomatic_Serial BOOLEAN DEFAULT false,                  /* Command all the heliostatts to automatic control mode */
    HeliostatsManual_Serial BOOLEAN DEFAULT false,                     /* Command all the heliostatts to manual control mode */
  ID_Serial VARCHAR(32) DEFAULT '',                                    /* The name of the serial communication bus */
  P$HFC_Serial INTEGER DEFAULT 0,                                      /* Pointer to the parent Heliostat Field Controller of the Serial Bus */
  TYPE_Serial INTEGER DEFAULT 0,                                       /* The type of serial bus */
    ModbusA_Serial BOOLEAN DEFAULT false,                              /* The serial bus is a MODBUS A circuit */
    ModbusB_Serial BOOLEAN DEFAULT false,                              /* The serial bus is a MODBUS B circuit */
    ModbusC_Serial BOOLEAN DEFAULT false,                              /* The serial bus is a MODBUS C circuit */
    ModbusD_Serial BOOLEAN DEFAULT false,                              /* The serial bus is a MODBUS D circuit */
  XlocationHFC_Serial DOUBLE PRECISION DEFAULT 0.0,                    /* The ENU East coordinate of the serial bus controller (m) */
  YlocationHFC_Serial DOUBLE PRECISION DEFAULT 0.0,                    /* The ENU North coordinate of the serial bus controller (m) */
  P$HCU_Serial INTEGER DEFAULT 0,                                      /* Pointer to the Heliostat Controllers on this Serial Bus */
  CommStatus_Serial INTEGER DEFAULT 0,                                 /* Communication status of the Serial Bus */
    CommunicationsGood_Serial BOOLEAN DEFAULT false,                   /* Communications are up */
    CommunicationsUnknown_Serial BOOLEAN DEFAULT false,                /* Communications status cannot be determined */
    CommunicationsBad_Serial BOOLEAN DEFAULT false,                    /* Communications are down */
  IDCableSourcePDU_Serial VARCHAR(20) DEFAULT '',                      /* The EPC name of the cable supplying High Voltage Power to the Heliostats */
  IDCableSourceUPS_Serial VARCHAR(20) DEFAULT ''                       /* The EPC name of the cable supplying High Voltage Power to the Heliostats */
);
  
/* A Heliostat Controller */
CREATE TABLE HCU(
  Instance_HCU INTEGER PRIMARY KEY,                                    /* Primary Key for the HCU Table */
  ID_HCU VARCHAR(32) DEFAULT '',                                       /* The name of the Heliostat Controller */
  Xlocation_HCU DOUBLE PRECISION DEFAULT 0.0,                          /* The ENU East coordinate of the Heliostat Control Unit (m) */
  Ylocation_HCU DOUBLE PRECISION DEFAULT 0.0,                          /* The ENU North coordinate of the Heliostat Control Unit (m) */
  I$Heliostat_HCU INTEGER DEFAULT 0,                                   /* The heliostat at the heliostat controller */
  P$Serial_HCU INTEGER DEFAULT 0,                                      /* The parent serial bus of this Heliostat Control Unit */
  Type_HCU INTEGER DEFAULT 0,                                          /* The type of control unit */
    ModbusA_HCU BOOLEAN DEFAULT false,                                 /* The unit is a device on MODBUS A */
    ModbusB_HCU BOOLEAN DEFAULT false,                                 /* The unit is a device on MODBUS B */
    ModbusC_HCU BOOLEAN DEFAULT false,                                 /* The unit is a device on MODBUS C */
    ModbusD_HCU BOOLEAN DEFAULT false,                                 /* The unit is a device on MODBUS D */
    HFC_HCU BOOLEAN DEFAULT false,                                     /* A Heliostat Field Control Unit Bus Controller */
    Heliostat_HCU BOOLEAN DEFAULT false,                               /* A Heliostat Control Unit */
    Spare_HCU BOOLEAN DEFAULT false,                                   /* (Spare) */
    WeatherStation_HCU BOOLEAN DEFAULT false,                          /* A Weatherstation Control Unit */
    Video_HCU BOOLEAN DEFAULT false,                                   /* A Camera Control Unit */
  Device_HCU INTEGER DEFAULT 0,                                        /* The MODBUS device address of this HCU */
  CommStatus_HCU INTEGER DEFAULT 0,                                    /* Communication status of the HCU */
    CommunicationsGood_HCU BOOLEAN DEFAULT false,                      /* Communications are up */
    CommunicationsUnknown_HCU BOOLEAN DEFAULT false,                   /* Communications status cannot be determined */
    CommunicationsBad_HCU BOOLEAN DEFAULT false,                       /* Communications are down */
    InvalidRead_HCU BOOLEAN DEFAULT false,                             /* The last read of analog data from the HCU failed */
    InvalidWrite_HCU BOOLEAN DEFAULT false,                            /* The last write of setpoint data to the HCU failed */
  CountFailRead_HCU INTEGER DEFAULT 0,                                 /* The count of failed analog data reads from the HCU. */
  CountFailWrite_HCU INTEGER DEFAULT 0,                                /* The count of failed setpoint data writes to the HCU */
  IOneLine_HCU INTEGER DEFAULT 0,                                      /* The lateral coordinate of the HCU in the oneline display (pixels) */
  JOneLine_HCU INTEGER DEFAULT 0,                                      /* The vertical coordinate of the HCU in the oneline display (pixels) */
  ScaleRS485OneLine_HCU DOUBLE PRECISION DEFAULT 0.0,                  /* Scaling to automatically place the HCU on the oneline display (future) */
  AngleRS485OneLine_HCU INTEGER DEFAULT 0,                             /* Angle to automatically place the HCU on the oneline display (future) */
  OrderRS485Wiring_HCU INTEGER DEFAULT 0,                              /* The wiring order of the HCU in the serial bus (If the EPC has to modify the order) */
  WiringRS485$HCU_HCU INTEGER DEFAULT 0,                               /* The HCU replacement for the HCU if the EPC has modified the wiring order */
  WiringRS485$Heliostat_HCU INTEGER DEFAULT 0                          /* The Heliostat assigned to the HCU that raplacement if the EPC has modified the wiring order */
);
  
/* HCU Drive Error History for this Heliostat */
CREATE TABLE DriveError(
  Instance_DriveError INTEGER PRIMARY KEY,                             /* Primary Key for the DriveError Table */
  TimeTagOccurred_DriveError TIMESTAMPTZ DEFAULT now(),                /* The Time when the drive error was detected by the HCU */
  TimeTagLogged_DriveError TIMESTAMPTZ DEFAULT now(),                  /* The Time when the drive error was reported by the HFCS */
  I$HCFaultCode_DriveError INTEGER DEFAULT 0,                          /* Pointer to the HCU Fault Code associated with the "Drive" Error */
  I$HCReportCode_DriveError INTEGER DEFAULT 0,                         /* Pointer to the HCU Report Code associated with the "Drive" Error */
  Retrieved_DriveError INTEGER DEFAULT 0,                              /* The data part retrieved */
    YearRetrieved_DriveError BOOLEAN DEFAULT false,                    /* The data holds the year when the error occurred */
    TimeOfYearRetrieved_DriveError BOOLEAN DEFAULT false,              /* The data holds the time when the error occurred */
    CodeRetrieved_DriveError BOOLEAN DEFAULT false                     /* The data holds the drive error code */
);
  
/* HCU Report Code */
CREATE TABLE HCReportCode(
  Instance_HCReportCode INTEGER PRIMARY KEY,                           /* Primary Key for the HCReportCode Table */
  ID_HCReportCode VARCHAR(8) DEFAULT '',                               /* The name of the HCReportCode (We use the Delta number) */
  Definition_HCReportCode VARCHAR(80) DEFAULT '',                      /* Definition of the Delta Report Code */
  Gain_HCReportCode DOUBLE PRECISION DEFAULT 0.0,                      /* Multiplier for the value given by Delta */
  Units_HCReportCode VARCHAR(20) DEFAULT '',                           /* Units for value */
  Type_HCReportCode INTEGER DEFAULT 0,                                 /* The report code word (32-bits) coming from the HCU */
    Analog_HCReportCode BOOLEAN DEFAULT false,                         /* This report code returns an analog value */
    Version_HCReportCode BOOLEAN DEFAULT false,                        /* This report code holds a vendor version number */
    VersionData_HCReportCode BOOLEAN DEFAULT false,                    /* This report code holds a vendor version addition al data */
    FaultCode_HCReportCode BOOLEAN DEFAULT false,                      /* This report code corresponds to a drive fault */
    HistoryFault_HCReportCode BOOLEAN DEFAULT false,                   /* This report code corresponds to a fault in the HCU history */
    HistoryYear_HCReportCode BOOLEAN DEFAULT false,                    /* This report code holds a defines the year of th fault */
    HistoryTime_HCReportCode BOOLEAN DEFAULT false                     /* This report code holds a defines the time of th fault */
);
  
/* HCU Fault Code */
CREATE TABLE HCFaultCode(
  Instance_HCFaultCode INTEGER PRIMARY KEY,                            /* Primary Key for the HCFaultCode Table */
  ID_HCFaultCode VARCHAR(8) DEFAULT '',                                /* The Delta error code such as 310, 300, 226 etc */
  Cause_HCFaultCode VARCHAR(80) DEFAULT '',                            /* The possible cause of the error code */
  Description_HCFaultCode VARCHAR(200) DEFAULT ''                      /* More detail of the possible cause of the error code */
);
  
/* A Camera Video Assembley accessed by ethernet */
CREATE TABLE Video(
  Instance_Video INTEGER PRIMARY KEY,                                  /* Primary Key for the Video Table */
  ID_Video VARCHAR(32) DEFAULT '',                                     /* The name of the Video Camera Assembly */
  TagFTP_Video VARCHAR(16) DEFAULT '',                                 /* The tag for the field control panel of this video */
  TagCamera_Video VARCHAR(16) DEFAULT '',                              /* The tag for the camera */
  TagPTU_Video VARCHAR(16) DEFAULT '',                                 /* The tag for the pan/tilt unit */
  Equipment_Video VARCHAR(16) DEFAULT '',                              /* The Equipment ID for this camera */
  Latitude_Video DOUBLE PRECISION DEFAULT 0.0,                         /* The surveyed WGS84 latitude of the camera foundation (degrees) */
  Longitude_Video DOUBLE PRECISION DEFAULT 0.0,                        /* The surveyed WGS84 longitude of the camera foundation (degrees) */
  Elevation_Video DOUBLE PRECISION DEFAULT 0.0,                        /* The surveyed WGS84 elevation of the camera foundation (m) */
  ElevationPTU_Video DOUBLE PRECISION DEFAULT 0.0,                     /* The elevation above ground level of the pivot point of the Pan/Tilt Unit (m) */
  HeightVerticalPTU_Video DOUBLE PRECISION DEFAULT 0.0,                /* The height of the pan/tilt unit when it is oriented vertically (m) */
  HeightCamera_Video DOUBLE PRECISION DEFAULT 0.0,                     /* The Height of the camera (m) */
  XOffsetCamera_Video DOUBLE PRECISION DEFAULT 0.0,                    /* The Horizontal Offset of the camera from the PTU pivot point (m) */
  YOffsetCamera_Video DOUBLE PRECISION DEFAULT 0.0,                    /* The Normal Offset of the camera from the PTU pivot point (m) */
  ZOffsetCamera_Video DOUBLE PRECISION DEFAULT 0.0,                    /* The Vertical Offset of the camera from the PTU pivot point (m) */
  AngleOrientationPTU_Video DOUBLE PRECISION DEFAULT 0.0,              /* The orientation of the pan/tilt unit from vertical (degrees) */
  AngleAzimuthalPTU_Video DOUBLE PRECISION DEFAULT 0.0,                /* The azimuthal rotation of the pan/tilt unit (degrees) */
  AngleElevationPTU_Video DOUBLE PRECISION DEFAULT 0.0,                /* The elevation rotation of the pan/tilt unit (degrees */
  Address_Video VARCHAR(32) DEFAULT '',                                /* The TCP/IP Address of the Camera */
  AddressPTU_Video VARCHAR(32) DEFAULT '',                             /* The TCP/IP Address of the Camera Pan/Tilt Unit */
  dLatitude_Video DOUBLE PRECISION DEFAULT 0.0,                        /* The surveyed Northing coordinate of the Camera foundation (us survey feet) */
  dLongitude_Video DOUBLE PRECISION DEFAULT 0.0,                       /* The surveyed Easting coordinate of the Camera foundation (us survey feet) */
  XLocation_Video DOUBLE PRECISION DEFAULT 0.0,                        /* The ENU East coordinate of the Camera foundation (m) */
  YLocation_Video DOUBLE PRECISION DEFAULT 0.0,                        /* The ENU North coordinate of the Camera foundation (m) */
  ZLocation_Video DOUBLE PRECISION DEFAULT 0.0,                        /* The ENU Up coordinate of the Camera foundation (m) */
  P$Ethernet_Video INTEGER DEFAULT 0,                                  /* Pointer to the parent ethernet bus of the heliostat field controller */
  P$HFC_Video INTEGER DEFAULT 0,                                       /* Pointer to the parent heliostat field controller */
  I$IRCamera_Video INTEGER DEFAULT 0,                                  /* The associated IR Camera */
  I$BCS_Video INTEGER DEFAULT 0,                                       /* The associated BCS Camera */
  Type_Video INTEGER DEFAULT 0,                                        /* The type of video camera */
    IRCameraVideo_Video BOOLEAN DEFAULT false,                         /* An infrared camera */
    BCSCameraVideo_Video BOOLEAN DEFAULT false,                        /* A BCS camera */
  P$Focus_Video INTEGER DEFAULT 0,                                     /* Pointer to the focusing events for this camera */
  P$BlockImage_Video INTEGER DEFAULT 0,                                /* Pointer to the BlockImage records for the heliostats blocking this camera */
  P$Picture_Video INTEGER DEFAULT 0,                                   /* Pointer to the picture events for this camera */
  P$Camera_Video INTEGER DEFAULT 0                                     /* Pointer to the camera equipment installation/replacement events */
);
  
/* A Camera focusing event */
CREATE TABLE Focus(
  Instance_Focus INTEGER PRIMARY KEY,                                  /* Primary Key for the Focus Table */
  TimeString_Focus VARCHAR(24) DEFAULT '',                             /* The time/date string when the focus occcurred */
  TimeTagHab_Focus TIMESTAMPTZ DEFAULT now(),                          /* The Time for the focusing */
  w0UpperLeft_Focus INTEGER DEFAULT 0,                                 /* The nominal width coordinate of the upper left edge of the target (pixels) */
  d0UpperLeft_Focus INTEGER DEFAULT 0,                                 /* The nominal depth coordinate of the upper left edge of the target (pixels) */
  w0UpperRight_Focus INTEGER DEFAULT 0,                                /* The nominal width coordinate of the upper right edge of the target (pixels) */
  d0UpperRight_Focus INTEGER DEFAULT 0,                                /* The nominal depth coordinate of the upper right edge of the target (pixels) */
  w0LowerRight_Focus INTEGER DEFAULT 0,                                /* The nominal width coordinate of the lower right edge of the target (pixels) */
  d0LowerRight_Focus INTEGER DEFAULT 0,                                /* The nominal depth coordinate of the lower right edge of the target (pixels) */
  w0LowerLeft_Focus INTEGER DEFAULT 0,                                 /* The nominal width coordinate of the lower left edge of the target (pixels) */
  d0LowerLeft_Focus INTEGER DEFAULT 0,                                 /* The nominal depth coordinate of the lower left edge of the target (pixels) */
  w0Center_Focus INTEGER DEFAULT 0,                                    /* The nominal width coordinate of the center of the target (pixels) */
  d0Center_Focus INTEGER DEFAULT 0,                                    /* The nominal depth coordinate of the center of the target (pixels) */
  CornerStart_Focus INTEGER DEFAULT 0,                                 /* The starting corner to process in edge detection */
  Wcenter_Focus INTEGER DEFAULT 0,                                     /* Nominal width coordinate of the center in the focused image (pixels) */
  Hcenter_Focus INTEGER DEFAULT 0,                                     /* Nominal width coordinate of the center in the focused image (pixels) */
  dWdX_Focus DOUBLE PRECISION DEFAULT 0.0,                             /* The number of pixels per meter in the focused image in the X-axis */
  dHdY_Focus DOUBLE PRECISION DEFAULT 0.0                              /* The number of pixes per meter in the focused  image in the Y-axis */
);
  
/* Heliostats to be moved out of the way of IR or BCS cameras */
CREATE TABLE BlockImage(
  Instance_BlockImage INTEGER PRIMARY KEY,                             /* Primary Key for the BlockImage Table */
  I$Heliostat_BlockImage INTEGER DEFAULT 0,                            /* Pointer to the heliostats blocking this camera */
  AngleAz_BlockImage DOUBLE PRECISION DEFAULT 0.0,                     /* Azimuthal position to prevent heliostat from blocking camera */
  AngleEl_BlockImage DOUBLE PRECISION DEFAULT 0.0                      /* Elevation position to prevent heliostat from blocking camera */
);
  
/* A Camera picture event */
CREATE TABLE Picture(
  Instance_Picture INTEGER PRIMARY KEY,                                /* Primary Key for the Picture Table */
  MetricCalculationBCS_Picture DOUBLE PRECISION DEFAULT 0.0,           /* The metric for selecting heliostat for this slot has been calculated */
  PrepareBCS$Heliostat_Picture INTEGER DEFAULT 0,                      /* The list of heliostats are being prepared to move onto the target */
  ProcessBCS$Heliostat_Picture INTEGER DEFAULT 0,                      /* The heliostat currently being processed in this slot */
  Completion$Heliostat_Picture INTEGER DEFAULT 0,                      /* The heliostat that last completed BCS processing for this slot */
  CompletionBCSHeliostat_Picture INTEGER DEFAULT 0,                    /* The status of a heliostats scheduled to have a BCS Image taken in this slot */
    Scheduled_Picture BOOLEAN DEFAULT false,                           /* The heliostat is scheduled to have an image taken */
    OnTarget_Picture BOOLEAN DEFAULT false,                            /* The heliostat is currently on the target */
    PictureTaken_Picture BOOLEAN DEFAULT false,                        /* The image of the heliostat was created */
    CentroidAcceptable_Picture BOOLEAN DEFAULT false                   /* The image was acceptable for later centroid processing */
);
  
/* A video image */
CREATE TABLE Image(
  Instance_Image INTEGER PRIMARY KEY,                                  /* Primary Key for the Image Table */
  ID_Image VARCHAR(32) DEFAULT '',                                     /* The name of a image */
  Path_Image VARCHAR(128) DEFAULT '',                                  /* The file path where the image data is stored */
  TimeTagHab_Image TIMESTAMPTZ DEFAULT now(),                          /* The time when the image was captured */
  State_Image INTEGER DEFAULT 0,                                       /* The processing state of the image. */
    TargetAreaExtracted_Image BOOLEAN DEFAULT false,                   /* The target area was found in the image */
    CentroidFound_Image BOOLEAN DEFAULT false,                         /* The centroid was calculated for the image */
    DegenerateLinearEstimate_Image BOOLEAN DEFAULT false,              /* The linear estimate of the target bottom failed */
    ScaleTestFailure_Image BOOLEAN DEFAULT false,                      /* The image edge length ratio test failed */
    StarkRatioTestFailure_Image BOOLEAN DEFAULT false,                 /* The image dark to light pixel ratio test failed */
    CornersShiftFailure_Image BOOLEAN DEFAULT false,                   /* The image corner locations shifted too much */
    AngleDeviationFailure_Image BOOLEAN DEFAULT false,                 /* The image corner angle test failed */
    AreaRatioTestFailure_Image BOOLEAN DEFAULT false,                  /* The image area to target ratio test failed */
  Wul_Image DOUBLE PRECISION DEFAULT 0.0,                              /* The upper left corner hoizontal coordinate (pixels) */
  Wur_Image DOUBLE PRECISION DEFAULT 0.0,                              /* The upper right corner hoizontal coordinate (pixels) */
  Wlr_Image DOUBLE PRECISION DEFAULT 0.0,                              /* The lower right corner hoizontal coordinate (pixels) */
  Wll_Image DOUBLE PRECISION DEFAULT 0.0,                              /* The lower left corner hoizontal coordinate (pixels) */
  Dul_Image DOUBLE PRECISION DEFAULT 0.0,                              /* The upper left corner vertical down coordinate (pixels) */
  Dur_Image DOUBLE PRECISION DEFAULT 0.0,                              /* The upper right corner vertical down coordinate (pixels) */
  Dlr_Image DOUBLE PRECISION DEFAULT 0.0,                              /* The lower right corner vertical down coordinate (pixels) */
  Dll_Image DOUBLE PRECISION DEFAULT 0.0,                              /* The lower left corner vertical down coordinate (pixels) */
  IntensityMaxTargetArea_Image DOUBLE PRECISION DEFAULT 0.0,           /* The maximum intensity in the extracted target area (intensity) */
  IntensityMinTargetArea_Image DOUBLE PRECISION DEFAULT 0.0,           /* The minimum intensity in the extracted target area (intensity) */
  FluxDNI_Image DOUBLE PRECISION DEFAULT 0.0,                          /* The Solar DNI flux when the image was captured (watts/m^2) */
  SpeedWind_Image DOUBLE PRECISION DEFAULT 0.0,                        /* The windspeed when the image was captured (m/s) */
  EncoderAzCount_Image INTEGER DEFAULT 0,                              /* The azimuthal drive encoder counts when the image was captured (counts) */
  EncoderElCount_Image INTEGER DEFAULT 0,                              /* The elevation drive encoder counts when the image was captured (counts) */
  I$Heliostat_Image INTEGER DEFAULT 0,                                 /* The pointer to the heliostat of the image */
  P$ColImg_Image INTEGER DEFAULT 0,                                    /* The pointer to the child image columns of this image */
  P$Video_Image INTEGER DEFAULT 0                                      /* Pointer to the parent video (e.g. BCS-North) */
);
  
/* Column of a video image */
CREATE TABLE ColImg(
  Instance_ColImg INTEGER PRIMARY KEY,                                 /* Primary Key for the ColImg Table */
  P$RowImg_ColImg INTEGER DEFAULT 0                                    /* The pointer to the child rows of each image column */
);
  
/* Row values of a video image */
CREATE TABLE RowImg(
  Instance_RowImg INTEGER PRIMARY KEY,                                 /* Primary Key for the RowImg Table */
  Intensity_RowImg INTEGER DEFAULT 0,                                  /* The camera image intensity data stored in row format */
  Type_RowImg INTEGER DEFAULT 0,                                       /* The type of image point classification made by the operator */
    CornerUpperLeft_RowImg BOOLEAN DEFAULT false,                      /* The point is an upper left target corner */
    CornerLowerLeft_RowImg BOOLEAN DEFAULT false,                      /* The point is an lower left target corner */
    CornerUpperRight_RowImg BOOLEAN DEFAULT false,                     /* The point is an upper right target corner */
    CornerLowerRight_RowImg BOOLEAN DEFAULT false,                     /* The point is an lower right target corner */
    BoundPolygonPoint_RowImg BOOLEAN DEFAULT false,                    /* The point is a vertex of the image bounding polygon */
    Centroid_RowImg BOOLEAN DEFAULT false                              /* The point is a centroid of the image */
);
  
/* Camera installation */
CREATE TABLE Camera(
  Instance_Camera INTEGER PRIMARY KEY,                                 /* Primary Key for the Camera Table */
  ID_Camera VARCHAR(32) DEFAULT '',                                    /* The Gigabit Ethernet name of the camera */
  TimeTagHab_Camera TIMESTAMPTZ DEFAULT now(),                         /* The time when the camera has been installed */
  WmaxImage_Camera INTEGER DEFAULT 0,                                  /* The width of the camera image (pixels) */
  HmaxImage_Camera INTEGER DEFAULT 0                                   /* The height of the camera image (pixels) */
);
  
/* A Weather Meter Instrument */
CREATE TABLE WeatherMeter(
  Instance_WeatherMeter INTEGER PRIMARY KEY,                           /* Primary Key for the WeatherMeter Table */
  FluxDNI_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                   /* Measurement of direct normal incident radiation (Watts/m^2) */
  Visibility_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                /* Measurement of atmospheric visibility (km) */
  SpeedWind_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                 /* Measurement of wind speed (m/s) */
  AngleDirection_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,            /* Direction of windspeed, measured clockwise from north (degrees) */
  SecondsAboveLimit_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,         /* Seconds the wind speed above the limit level (seconds) */
  Psia_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                      /* Measurement of barometric pressure (Psia) */
  RelativeHumidity_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,          /* Measurement of relative humidity (Percent) */
  SpeedWindPeak_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,             /* Peak wind speed (m/s) */
  Precipitation_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,             /* Rainfall over the past hour (cm) */
  IntensityRainfall_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,         /* Rain Intensity (cm/hr) */
  TemperatureF_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,              /* Ambient Temperature (DegF) */
  TemperatureC_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,              /* Ambient Temperature (DegC) */
  Temperature_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,               /* Ambient Temperature (System Units) */
  Status_WeatherMeter INTEGER DEFAULT 0,                               /* The status of the measurements from the weathermeter */
    NormalFluxDNI_WeatherMeter BOOLEAN DEFAULT false,                  /* The pyroheliometer measurement is good. */
    NormalVisibility_WeatherMeter BOOLEAN DEFAULT false,               /* The visibility measurement is good. */
    NormalWindspeed_WeatherMeter BOOLEAN DEFAULT false,                /* The windspeed measurement is good. */
    NormalWindAngle_WeatherMeter BOOLEAN DEFAULT false,                /* The wind direction measurement is good. */
    NormalPressure_WeatherMeter BOOLEAN DEFAULT false,                 /* The atmospheric pressure measurement is good. */
    NormalHumidity_WeatherMeter BOOLEAN DEFAULT false,                 /* The humidity measurement is good. */
    NormalWindPeak_WeatherMeter BOOLEAN DEFAULT false,                 /* The peak windspeed measurement is good. */
    NormalRainFall_WeatherMeter BOOLEAN DEFAULT false,                 /* The rainfall measurement is good. */
    NormalIntensity_WeatherMeter BOOLEAN DEFAULT false,                /* The rainfall intensity measurement is good. */
    NormalTemperature_WeatherMeter BOOLEAN DEFAULT false,              /* The temperature measurement is good */
    FaultFluxDNI_WeatherMeter BOOLEAN DEFAULT false,                   /* The pyroheliometer measurement is bad. */
    FaultVisibility_WeatherMeter BOOLEAN DEFAULT false,                /* The visibility measurement is bad. */
    FaultWindspeed_WeatherMeter BOOLEAN DEFAULT false,                 /* The windspeed measurement is bad. */
    FaultWindAngle_WeatherMeter BOOLEAN DEFAULT false,                 /* The wind direction measurement is bad. */
    FaultPressure_WeatherMeter BOOLEAN DEFAULT false,                  /* The atmospheric pressure measurement is bad. */
    FaultHumidity_WeatherMeter BOOLEAN DEFAULT false,                  /* The humidity measurement is bad. */
    FaultWindPeak_WeatherMeter BOOLEAN DEFAULT false,                  /* The peak windspeed measurement is bad. */
    FaultRainFall_WeatherMeter BOOLEAN DEFAULT false,                  /* The rainfall measurement is bad. */
    FaultIntensity_WeatherMeter BOOLEAN DEFAULT false,                 /* The rainfall intensity measurement is bad. */
    FaultTemperature_WeatherMeter BOOLEAN DEFAULT false,               /* The temperature measurement is bad */
    ReplaceFluxDNI_WeatherMeter BOOLEAN DEFAULT false,                 /* The pyroheliometer measurement has been manually replaced. */
    ReplaceVisibility_WeatherMeter BOOLEAN DEFAULT false,              /* The visibility measurement has been manually replaced. */
    ReplaceWindspeed_WeatherMeter BOOLEAN DEFAULT false,               /* The windspeed measurement has been manually replaced. */
    ReplaceWindAngle_WeatherMeter BOOLEAN DEFAULT false,               /* The wind direction measurement has been manually replaced. */
    ReplacePressure_WeatherMeter BOOLEAN DEFAULT false,                /* The atmospheric pressure measurement has been manually replaced. */
    ReplaceHumidity_WeatherMeter BOOLEAN DEFAULT false,                /* The humidity measurement has been manually replaced. */
    ReplaceWindPeak_WeatherMeter BOOLEAN DEFAULT false,                /* The peak windspeed measurement has been manually replaced. */
    ReplaceRainFall_WeatherMeter BOOLEAN DEFAULT false,                /* The rainfall measurement has been manually replaced. */
    ReplaceIntensity_WeatherMeter BOOLEAN DEFAULT false,               /* The rainfall intensity measurement has been manually replaced. */
    ReplaceTemperature_WeatherMeter BOOLEAN DEFAULT false,             /* The temperature measurement has been manually replaced */
  ID_WeatherMeter VARCHAR(32) DEFAULT '',                              /* The name of the Weathermeter */
  Equipment_WeatherMeter VARCHAR(16) DEFAULT '',                       /* The Equipment tag for this weathermeter */
  Latitude_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                  /* The surveyed WGS84 latitude of the Weather Station foundation (degrees) */
  Longitude_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                 /* The surveyed WGS84 longitude of the Weather Station foundation  (degrees) */
  Elevation_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                 /* The surveyed WGS84 elevation of the Weather Station foundation (m) */
  dLatitude_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                 /* The surveyed Northing coordinate of the Weather Station foundation (us survey feet) */
  dLongitude_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                /* The surveyed Easting coordinate of the Weather Station foundation (us survey feet) */
  XLocation_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                 /* The ENU East coordinate of the Weather Station foundation (m) */
  YLocation_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                 /* The ENU North coordinate of the Weather Station foundation (m) */
  ZLocation_WeatherMeter DOUBLE PRECISION DEFAULT 0.0,                 /* The ENU Up coordinate of the Weather Station foundation (m) */
  P$HCU_WeatherMeter INTEGER DEFAULT 0,                                /* Pointer to the parent controller of the weathermeter */
  Type_WeatherMeter INTEGER DEFAULT 0,                                 /* The instruments on the weathermeter */
    Pyrheliometer_WeatherMeter BOOLEAN DEFAULT false,                  /* The weathermeter measures Solar DNI */
    Transmissometer_WeatherMeter BOOLEAN DEFAULT false,                /* The weathermeter measures visibility */
    Anemometer_WeatherMeter BOOLEAN DEFAULT false,                     /* The weathermeter measures wind speed */
    WindVane_WeatherMeter BOOLEAN DEFAULT false,                       /* The weathermeter measures wind direction */
    Barometer_WeatherMeter BOOLEAN DEFAULT false,                      /* The weathermeter measures atmospheric pressure */
    Hygrometer_WeatherMeter BOOLEAN DEFAULT false,                     /* The weathermeter measures humidity */
    AnemometerPeak_WeatherMeter BOOLEAN DEFAULT false,                 /* The weathermeter measures peak wind speed */
    RainGauge_WeatherMeter BOOLEAN DEFAULT false,                      /* The weathermeter measures rain accumulation */
    Disdrometer_WeatherMeter BOOLEAN DEFAULT false,                    /* The weathermeter measures rain intensity */
    Thermocouple_WeatherMeter BOOLEAN DEFAULT false                    /* The weathermeter measures ambient temperature */
);
  
/* Scada Analogs */
CREATE TABLE Analog(
  Instance_Analog INTEGER PRIMARY KEY,                                 /* Primary Key for the Analog Table */
  Key_Analog INTEGER DEFAULT 0,                                        /* SCADA Key for all the analogs values used by the HFCS */
  TimeTagHab_Analog TIMESTAMPTZ DEFAULT now(),                         /* The Time of telemetry data coming from the SCADA */
  Value_Analog DOUBLE PRECISION DEFAULT 0.0,                           /* The value of telemetry data coming from the SCADA */
  Quality_Analog INTEGER DEFAULT 0,                                    /* Quality of the telemetry data coming from SCADA */
    Normal_Analog BOOLEAN DEFAULT false,                               /* The telemetered data is good */
    Fault_Analog BOOLEAN DEFAULT false,                                /* The telemetered data is bad */
    Replaced_Analog BOOLEAN DEFAULT false                              /* The telemetered data has been manually replaced */
);
  
/* Scada Setpoints */
CREATE TABLE Setpnt(
  Instance_Setpnt INTEGER PRIMARY KEY,                                 /* Primary Key for the Setpnt Table */
  Key_Setpnt INTEGER DEFAULT 0,                                        /* SCADA Key for all the setpoint values used by the HFCS */
  TimeTagHab_Setpnt TIMESTAMPTZ DEFAULT now(),                         /* The Time of command data going to SCADA */
  Value_Setpnt DOUBLE PRECISION DEFAULT 0.0                            /* The value of command data going to SCADA */
);
  
/* Tag Names for integrating with SCADA systems */
CREATE TABLE TagName(
  Instance_TagName INTEGER PRIMARY KEY,                                /* Primary Key for the TagName Table */
  ID_TagName VARCHAR(24) DEFAULT '',                                   /* The ID of the tag */
  I$Setpnt_TagName INTEGER DEFAULT 0,                                  /* The associated setpoint of the tag */
  I$Analog_TagName INTEGER DEFAULT 0,                                  /* The associated analog of the tag */
  Type_TagName INTEGER DEFAULT 0,                                      /* The type of tag */
    Analog_TagName BOOLEAN DEFAULT false,                              /* The tag is an analog measurement */
    Setpnt_TagName BOOLEAN DEFAULT false                               /* The tag is a setpoing */
);
  
/* Power Links for each HFC */
CREATE TABLE PwrLink(
  Instance_PwrLink INTEGER PRIMARY KEY,                                /* Primary Key for the PwrLink Table */
  I$Power_PwrLink INTEGER DEFAULT 0,                                   /* Pointer to the power source associated with a linkage */
  Type_PwrLink INTEGER DEFAULT 0,                                      /* Type of Power Linkage */
    SourceToHFC_PwrLink BOOLEAN DEFAULT false,                         /* Provides power to the HFC */
    ControlledByHFC_PwrLink BOOLEAN DEFAULT false                      /* Controlled from inside the HFC enclosure */
);
  
/* Tasks running in the HFCS */
CREATE TABLE Task(
  Instance_Task INTEGER PRIMARY KEY,                                   /* Primary Key for the Task Table */
  ID_Task VARCHAR(32) DEFAULT '',                                      /* The name of the task */
  Heartbeat_Task INTEGER DEFAULT 0,                                    /* The task heartbeat */
  State_Task INTEGER DEFAULT 0,                                        /* The state of the task */
    Exit_Task BOOLEAN DEFAULT false,                                   /* The task has exited */
    Running_Task BOOLEAN DEFAULT false,                                /* The task is currently running */
    RoleUnknown_Task BOOLEAN DEFAULT false,                            /* The task has an unknown Role */
    RoleOnline_Task BOOLEAN DEFAULT false,                             /* The task has taken the Online Role */
    RoleStandby_Task BOOLEAN DEFAULT false,                            /* The task has taken the Standby Role */
    Debug_Task BOOLEAN DEFAULT false                                   /* Turn on debug for this task */
);
  
/* Parameter Values used in the HFCS */
CREATE TABLE Param(
  Instance_Param INTEGER PRIMARY KEY,                                  /* Primary Key for the Param Table */
  ID_Param VARCHAR(32) DEFAULT '',                                     /* The name of the parameter */
  Type_Param INTEGER DEFAULT 0,                                        /* This defines the parameter type */
    Integer_Param BOOLEAN DEFAULT false,                               /* The parameter is integer type */
    Real_Param BOOLEAN DEFAULT false,                                  /* The parameter is real type */
    Logical_Param BOOLEAN DEFAULT false,                               /* The parameter is logical type */
    Character_Param BOOLEAN DEFAULT false,                             /* The parameter is character type */
    Spare_Param BOOLEAN DEFAULT false,                                 /* Future expansion */
  IVal_Param INTEGER DEFAULT 0,                                        /* The integer value of the parameter */
  RVal_Param DOUBLE PRECISION DEFAULT 0.0,                             /* The real value of the parameter */
  LVal_Param BOOLEAN DEFAULT false,                                    /* The logical value of the parameter */
  CVal_Param VARCHAR(256) DEFAULT '',                                  /* The character value of the parameter */
  IvalMin_Param INTEGER DEFAULT 0,                                     /* The minimum value of an integer parameter */
  IvalMax_Param INTEGER DEFAULT 0,                                     /* The maximum value of an integer parameter */
  RValMin_Param DOUBLE PRECISION DEFAULT 0.0,                          /* The minimum value of an real parameter */
  RValMax_Param DOUBLE PRECISION DEFAULT 0.0                           /* The maximum value of an real parameter */
);
  
/* Power Source for the Heliostat Field */
CREATE TABLE Power(
  Instance_Power INTEGER PRIMARY KEY,                                  /* Primary Key for the Power Table */
  State_Power INTEGER DEFAULT 0,                                       /* The state of the power sources for the field and receiver */
    Available_Power BOOLEAN DEFAULT false,                             /* The power source is available */
    Unavailable_Power BOOLEAN DEFAULT false,                           /* The power source is unavailable */
  SecondsAvailable_Power INTEGER DEFAULT 0,                            /* The number of seconds since the power source has been available */
  ID_Power VARCHAR(32) DEFAULT '',                                     /* The name of the Power Center */
  Description_Power VARCHAR(48) DEFAULT '',                            /* The description of the Power Center */
  IDTransformerUPS_Power VARCHAR(32) DEFAULT '',                       /* The name of the transformer powering the UPS */
  Communication$HFC_Power INTEGER DEFAULT 0,                           /* The HFC providing telemetry for the Power Center */
  From$Connect_Power INTEGER DEFAULT 0,                                /* The 'High' connection of the Power */
  To$Connect_Power INTEGER DEFAULT 0,                                  /* The 'Low' connection of the Power */
  From$Bus_Power INTEGER DEFAULT 0,                                    /* The 'High' Bus of the Power */
  To$Bus_Power INTEGER DEFAULT 0,                                      /* The 'Low' Bus of the Power */
  StatusSet_Power INTEGER DEFAULT 0,                                   /* UI Operator can toggle the state of the Power Sources (only in the Simulation Configuration) */
    ToggleSet_Power BOOLEAN DEFAULT false,                             /* Toggle the state of the Power Source in the Simulation Configuration */
  Type_Power INTEGER DEFAULT 0,                                        /* The type of power source */
    Source_Power BOOLEAN DEFAULT false,                                /* Utility Power */
    Transformer_Power BOOLEAN DEFAULT false,                           /* Transformer Power */
    UPS_Power BOOLEAN DEFAULT false,                                   /* Battery Power */
  I$StatusPower_Power INTEGER DEFAULT 0,                               /* Pointer to Power Center Status Word */
  BitInStatusPower_Power INTEGER DEFAULT 0                             /* Bit number in Power Center Status Word from the DCS */
);
  
/* Devices fed by source */
CREATE TABLE Feed(
  Instance_Feed INTEGER PRIMARY KEY,                                   /* Primary Key for the Feed Table */
  ID_Feed VARCHAR(32) DEFAULT '',                                      /* The name of the Feed */
  Powered$HFC_Feed INTEGER DEFAULT 0                                   /* The first downstream HFC powered by feed */
);
  
/* Power Bus for the Heliostat Field */
CREATE TABLE Bus(
  Instance_Bus INTEGER PRIMARY KEY,                                    /* Primary Key for the Bus Table */
  State_Bus INTEGER DEFAULT 0,                                         /* The state of a power system bus. */
    Energized_Bus BOOLEAN DEFAULT false,                               /* The bus is energized */
    Deenergized_Bus BOOLEAN DEFAULT false,                             /* The bus is not energized */
    Unknown_Bus BOOLEAN DEFAULT false,                                 /* The state of the bus is unknown */
  ID_Bus VARCHAR(32) DEFAULT '',                                       /* The name of the Power Bus */
  Description_Bus VARCHAR(48) DEFAULT '',                              /* The description of the Power Bus */
  VoltageNominal_Bus DOUBLE PRECISION DEFAULT 0.0,                     /* The nominal voltage for the Power Bus */
  I$StatusPower_Bus INTEGER DEFAULT 0,                                 /* Pointer to Power Center Status Word */
  BitInStatusPower_Bus INTEGER DEFAULT 0                               /* Bit number in Power Center Status Word */
);
  
/* Power Lines for the Heliostat Field */
CREATE TABLE Cable(
  Instance_Cable INTEGER PRIMARY KEY,                                  /* Primary Key for the Cable Table */
  ID_Cable VARCHAR(32) DEFAULT '',                                     /* The name of the Power Cable */
  Description_Cable VARCHAR(48) DEFAULT '',                            /* The description of the Power Cable */
  CircuitNumber_Cable INTEGER DEFAULT 0,                               /* The electrical circuit number of the cable run */
  From$Connect_Cable INTEGER DEFAULT 0,                                /* The 'From' connection of the cable */
  To$Connect_Cable INTEGER DEFAULT 0,                                  /* The 'To' connection of the cable */
  From$Bus_Cable INTEGER DEFAULT 0,                                    /* The 'From' Bus of the cable */
  To$Bus_Cable INTEGER DEFAULT 0                                       /* The 'To' Bus of the cable */
);
  
/* Power Connections for the Heliostat Field */
CREATE TABLE Connect(
  Instance_Connect INTEGER PRIMARY KEY,                                /* Primary Key for the Connect Table */
  State_Connect INTEGER DEFAULT 0,                                     /* The state of a connection (breaker or switch) in the power system */
    Open_Connect BOOLEAN DEFAULT false,                                /* The connection is open */
    Closed_Connect BOOLEAN DEFAULT false,                              /* The connection is closed */
    Unknown_Connect BOOLEAN DEFAULT false,                             /* The connection status is unknown */
  ID_Connect VARCHAR(32) DEFAULT '',                                   /* The name of the Power System connection */
  I$StatusPower_Connect INTEGER DEFAULT 0,                             /* Pointer to Power Center Status Word */
  BitInStatusPower_Connect INTEGER DEFAULT 0                           /* This entry should be moved to Operator Commands */
);
  
/* SCAPI work queue for processing through RUSER */
CREATE TABLE QueueSCAPI(
  Instance_QueueSCAPI INTEGER PRIMARY KEY,                             /* Primary Key for the QueueSCAPI Table */
  Oper_QueueSCAPI VARCHAR(32) DEFAULT '',                              /* The operation to be performed */
  CParm_QueueSCAPI VARCHAR(48) DEFAULT '',                             /* Any additional parameters needed for the operation */
  Ndx_QueueSCAPI INTEGER DEFAULT 0                                     /* The object index upon which the operation is to be performed */
);
  
/* Cameras work queue for processing through RUSER */
CREATE TABLE QueueCameras(
  Instance_QueueCameras INTEGER PRIMARY KEY,                           /* Primary Key for the QueueCameras Table */
  Oper_QueueCameras VARCHAR(32) DEFAULT '',                            /* The operation to be performed */
  CParm_QueueCameras VARCHAR(48) DEFAULT '',                           /* Any additional parameters needed for the operation */
  Ndx_QueueCameras INTEGER DEFAULT 0                                   /* The object index upon which the operation is to be performed */
);
  
/* Ephmrs work queue for processing through RUSER */
CREATE TABLE QueueEphmrs(
  Instance_QueueEphmrs INTEGER PRIMARY KEY,                            /* Primary Key for the QueueEphmrs Table */
  Oper_QueueEphmrs VARCHAR(32) DEFAULT '',                             /* The operation to be performed */
  CParm_QueueEphmrs VARCHAR(48) DEFAULT '',                            /* Any additional parameters needed for the operation */
  Ndx_QueueEphmrs INTEGER DEFAULT 0                                    /* The object index upon which the operation is to be performed */
);
  
/* One receiver energy balance for every 30 seconds from sunrise to sunset. */
CREATE TABLE Balance(
  Instance_Balance INTEGER PRIMARY KEY,                                /* Primary Key for the Balance Table */
  TimeTagHab_Balance TIMESTAMPTZ DEFAULT now(),                        /* The time of this energy balance interval */
  FluxSunDNI_Balance DOUBLE PRECISION DEFAULT 0.0,                     /* The DNI for this time (kw/m^2) */
  FluxSunClearSky_Balance DOUBLE PRECISION DEFAULT 0.0,                /* The clear sky DNI for this time (w/m^2) */
  SpeedWind_Balance DOUBLE PRECISION DEFAULT 0.0,                      /* The windspeed for this time (m/s) */
  QGrossSolar_Balance DOUBLE PRECISION DEFAULT 0.0,                    /* The total solar energy available to the field for this time (MW-thermal) */
  QLossShadow_Balance DOUBLE PRECISION DEFAULT 0.0,                    /* The shadowing losses in the field for this time (MW-thermal) */
  QLossCosine_Balance DOUBLE PRECISION DEFAULT 0.0,                    /* The cosine losses in the field for this time (MW-thermal) */
  QLossMirror_Balance DOUBLE PRECISION DEFAULT 0.0,                    /* The mirror reflectivity losses in the field for this time (MW-thermal) */
  QLossOffpoint_Balance DOUBLE PRECISION DEFAULT 0.0,                  /* The heliostat off-pointing losses in the field for this time (MW-thermal) */
  QLossBlocking_Balance DOUBLE PRECISION DEFAULT 0.0,                  /* The blocking losses in the field for this time (MW-thermal) */
  QLossAtmosphere_Balance DOUBLE PRECISION DEFAULT 0.0,                /* The atmospheric losses in the field for this time (MW-thermal) */
  QLossSpillage_Balance DOUBLE PRECISION DEFAULT 0.0,                  /* The spillage losses in the field for this time (MW-thermal) */
  QIncident_Balance DOUBLE PRECISION DEFAULT 0.0,                      /* The power incident on the receiver for this time (MW-Thermal) */
  QLossReflection_Balance DOUBLE PRECISION DEFAULT 0.0,                /* The power reflected off the receiver for this time (MW-Thermal) */
  QLossConduction_Balance DOUBLE PRECISION DEFAULT 0.0,                /* The power conduction losses to air from the receiver for this time (MW-Thermal) */
  QLossConvection_Balance DOUBLE PRECISION DEFAULT 0.0,                /* The power convection losses to air from the receiver for this time (MW-Thermal) */
  QLossRadiation_Balance DOUBLE PRECISION DEFAULT 0.0,                 /* The radiation losses from the receiver for this time (MW-Thermal) */
  QLossMetal_Balance DOUBLE PRECISION DEFAULT 0.0,                     /* The power loss/gain from the metal capacitance of the receiver for this time (MW-Thermal) */
  QLossPiping_Balance DOUBLE PRECISION DEFAULT 0.0,                    /* The piping losses in the receiver for this time (MW-Thermal) */
  Qsalt_Balance DOUBLE PRECISION DEFAULT 0.0,                          /* The power output of the receiver for this time (MW-Thermal) */
  WFlow_Balance DOUBLE PRECISION DEFAULT 0.0,                          /* The output flow rate of the receiver for this time (Lbms/second) */
  TempFIRMax_Balance DOUBLE PRECISION DEFAULT 0.0,                     /* The maximum IR camera receiver temperature for this time (degF) */
  BalanceFactorUsed_Balance DOUBLE PRECISION DEFAULT 0.0,              /* The balance factor used for this time  (fraction) */
  Qtelemetry_Balance DOUBLE PRECISION DEFAULT 0.0,                     /* Receiver power calculated from telemetry (increase in enthalpy of fluid) (MWt) */
  Qfiltered_Balance DOUBLE PRECISION DEFAULT 0.0,                      /* Receiver power calculated from filtered telemetry matching (increase in enthalpy of fluid) (MWt) */
  QReport_Balance DOUBLE PRECISION DEFAULT 0.0,                        /* Receiver power calculated from telemetry (increase in enthalpy of fluid) (MWt) */
  ReflectivityMeasured_Balance DOUBLE PRECISION DEFAULT 0.0,           /* Measured reflectivity for this time (fraction) */
  NumHeliostatsTracking_Balance INTEGER DEFAULT 0,                     /* Number of heliostats tracking for this time */
  NumHeliostatsOnReceiver_Balance INTEGER DEFAULT 0,                   /* Number of heliostats focused on the receiver for this time */
  StateReceiver_Balance INTEGER DEFAULT 0,                             /* The actual state of the receiver at this time */
    Fill_Balance BOOLEAN DEFAULT false,                                /* The Receiver was Filling */
    Operate_Balance BOOLEAN DEFAULT false,                             /* The Receiver was Operating */
    Drain_Balance BOOLEAN DEFAULT false,                               /* The Receiver was Draining */
    ShortTermHold_Balance BOOLEAN DEFAULT false,                       /* The Receiver was at Short Term Hold */
    LongTermHold_Balance BOOLEAN DEFAULT false,                        /* The Receiver was at Long Term Hold */
  StateField_Balance INTEGER DEFAULT 0,                                /* The state of the field at this time */
    Stow_Balance BOOLEAN DEFAULT false,                                /* The Field was commanded to the Wind Stow State */
    StandBy_Balance BOOLEAN DEFAULT false,                             /* The Field was commanded to the Standby State */
    PreHeat_Balance BOOLEAN DEFAULT false,                             /* The Field was commanded to the Preheat State */
    Generation_Balance BOOLEAN DEFAULT false,                          /* The Field was commanded to the Generation State */
    PostHeat_Balance BOOLEAN DEFAULT false,                            /* The Field was commanded to the Postheat State */
    Defocus_Balance BOOLEAN DEFAULT false,                             /* The Field was commanded to the Emergency Defocus State */
    Sleep_Balance BOOLEAN DEFAULT false,                               /* The Field was commanded to the Night Stow State */
    RainWash_Balance BOOLEAN DEFAULT false,                            /* The Field was commanded to the Rainwash State */
    Defrost_Balance BOOLEAN DEFAULT false,                             /* The Field was commanded to the Defrost State */
  Entered_Balance INTEGER DEFAULT 0,                                   /* For calculation and display purposes */
    ValidEntry_Balance BOOLEAN DEFAULT false                           /* Marks whether this 30 second interval had an energy balance calculated or not. */
);
  
/* Operator log in the Energy Balance Report */
CREATE TABLE LogBalance(
  Instance_LogBalance INTEGER PRIMARY KEY,                             /* Primary Key for the LogBalance Table */
  Message_LogBalance VARCHAR(120) DEFAULT ''                           /* Operator log message added to the Balance */
);
  
/* Power Center Status Word */
CREATE TABLE StatusPower(
  Instance_StatusPower INTEGER PRIMARY KEY,                            /* Primary Key for the StatusPower Table */
  Value_StatusPower INTEGER DEFAULT 0,                                 /* Value of Power Center Status Word */
  BitCommStatus_StatusPower INTEGER DEFAULT 0,                         /* Bit Number of Comm Status in Power Center Status Word */
  Status_StatusPower INTEGER DEFAULT 0,                                /* The status of the power availability measurement */
    Normal_StatusPower BOOLEAN DEFAULT false,                          /* The measurement is good */
    Fault_StatusPower BOOLEAN DEFAULT false,                           /* The measurement is bad */
    Replaced_StatusPower BOOLEAN DEFAULT false                         /* The measurement has been manually replaced */
);
  
/* Color scale for temperature, strain and flux maps */
CREATE TABLE Palette(
  Instance_Palette INTEGER PRIMARY KEY,                                /* Primary Key for the Palette Table */
  ID_Palette VARCHAR(32) DEFAULT '',                                   /* The name of a color palette */
  NameCAM_Palette VARCHAR(64) DEFAULT '',                              /* The name of the FG Display Builder CAM corresponding to this Color Palete */
  NameField_Palette VARCHAR(32) DEFAULT '',                            /* The name of the database field using this palette */
  P$Color_Palette INTEGER DEFAULT 0                                    /* Pointer to the colors used in this palette */
);
  
/* Color in a color scale */
CREATE TABLE Color(
  Instance_Color INTEGER PRIMARY KEY,                                  /* Primary Key for the Color Table */
  Red_Color INTEGER DEFAULT 0,                                         /* The red color value (0 - 255) */
  Green_Color INTEGER DEFAULT 0,                                       /* The green color value (0 - 255) */
  Blue_Color INTEGER DEFAULT 0,                                        /* The blue color value (0 - 255) */
  Intensity_Color DOUBLE PRECISION DEFAULT 0.0,                        /* The intensity corresponding to the RGB value */
  Value_Color DOUBLE PRECISION DEFAULT 0.0                             /* The engineering value corresponding to the color */
);
  
/* Notes for the operators */
CREATE TABLE BCSComment(
  Instance_BCSComment INTEGER PRIMARY KEY,                             /* Primary Key for the BCSComment Table */
  Note_BCSComment VARCHAR(80) DEFAULT '',                              /* Operator notes for the BCS. */
  P$BCSHistory_BCSComment INTEGER DEFAULT 0                            /* Parent BCS for this note. */
);
  
/* Earth Orientation Parameters */
CREATE TABLE IERSOrientation(
  Instance_IERSOrientation INTEGER PRIMARY KEY,                        /* Primary Key for the IERSOrientation Table */
  Year_IERSOrientation INTEGER DEFAULT 0,                              /* Year referenced for Earth Orientation Parameter update */
  Month_IERSOrientation INTEGER DEFAULT 0,                             /* Month referenced for Earth Orientation Parameter update */
  Day_IERSOrientation INTEGER DEFAULT 0,                               /* Day referenced for Earth Orientation Parameter update */
  dAngleX_IERSOrientation DOUBLE PRECISION DEFAULT 0.0,                /* X-axis rotation difference between the Celestial Ephemeris Pole and Internation Reference Pole (arc-seconds) */
  dAngleY_IERSOrientation DOUBLE PRECISION DEFAULT 0.0,                /* Y-axis rotation difference between the Celestial Ephemeris Pole and Internation Reference Pole (arc-seconds) */
  DUT1_IERSOrientation DOUBLE PRECISION DEFAULT 0.0                    /* Time difference between UTC and UT1 (seconds) */
);
  
/* Leap Second Notices */
CREATE TABLE IERSLeapSecond(
  Instance_IERSLeapSecond INTEGER PRIMARY KEY,                         /* Primary Key for the IERSLeapSecond Table */
  Second_IERSLeapSecond DOUBLE PRECISION DEFAULT 0.0,                  /* Second referenced for leap second notice */
  Minute_IERSLeapSecond INTEGER DEFAULT 0,                             /* Minute referenced for leap second notice */
  Hour_IERSLeapSecond INTEGER DEFAULT 0,                               /* Hour referenced for leap second notice */
  Day_IERSLeapSecond INTEGER DEFAULT 0,                                /* Day referenced for leap second notice */
  Month_IERSLeapSecond INTEGER DEFAULT 0,                              /* Month referenced for leap second notics */
  Year_IERSLeapSecond INTEGER DEFAULT 0                                /* Year referenced for leap second notice */
);
  
/* Scalar values used in the HFCS */
CREATE TABLE Items(
  Instance_Items INTEGER PRIMARY KEY,                                  /* Primary Key for the Items Table */
  VERSION_Items VARCHAR(32) DEFAULT '',                                /* The current version of control software installed */
  NumCommandBCSQueue_Items INTEGER DEFAULT 0,                          /* Number of unprocessed queue entries */
  SourceFluxBCS_Items INTEGER DEFAULT 0,                               /* The souce of light used for BCS targeting */
    SunFocusOnTarget_Items BOOLEAN DEFAULT false,                      /* Use the sun for BCS targeting */
    MoonFocusOnTarget_Items BOOLEAN DEFAULT false,                     /* Use the moon for BCS targeting */
    VenusFocusOnTarget_Items BOOLEAN DEFAULT false,                    /* Future feature */
    MarsFocusOnTarget_Items BOOLEAN DEFAULT false,                     /* Future feature */
    JupiterFocusOnTarget_Items BOOLEAN DEFAULT false,                  /* Future feature */
    StarFocusOnTarget_Items BOOLEAN DEFAULT false,                     /* Future feature */
  TimeTagHabAutoBCS_Items TIMESTAMPTZ DEFAULT now(),                   /* The time when the Autoatic BCS list of heliostats was updated */
  dZTargetReceiverMin_Items DOUBLE PRECISION DEFAULT 0.0,              /* Receiver Target Minimum Vertical Offset from the Center (m) */
  dZTargetReceiverMax_Items DOUBLE PRECISION DEFAULT 0.0,              /* Receiver Target Maximum Vertical Offset from the Center (m) */
  dXTargetReceiverMin_Items DOUBLE PRECISION DEFAULT 0.0,              /* Receiver Target Minimum Horizontal Offset from the Nominal Aimpoint (m) */
  dXTargetReceiverMax_Items DOUBLE PRECISION DEFAULT 0.0,              /* Receiver Target Maximum Horizontal Offset from the Nominal Center (m) */
  PercentDerateReceiver_Items DOUBLE PRECISION DEFAULT 0.0,            /* The flow rate de-rate as a percent of maximum rated flow (percent) */
  dTempLimitPanelPreheat_Items DOUBLE PRECISION DEFAULT 0.0,           /* The maximum panel to panel average delta temperature for preheat (degF) */
  TempAvgPanelMaxPreheat_Items DOUBLE PRECISION DEFAULT 0.0,           /* The maximum panel average temperature for preheat (degF) */
  TempAvgPanelMinPreheat_Items DOUBLE PRECISION DEFAULT 0.0,           /* The minimum panel average temperature for preheat (degF) */
  TempBackwallMaxPreheat_Items DOUBLE PRECISION DEFAULT 0.0,           /* The maximum backwall thermocouple temperature for preheat (degF) */
  TempBackwallMinPreheat_Items DOUBLE PRECISION DEFAULT 0.0,           /* The maximum backwall thermocouple temperature for preheat (degF) */
  dTempLimitFluidPreheat_Items DOUBLE PRECISION DEFAULT 0.0,           /* The maximum backwall thermocouple to fluid delta temperature for preheat (degF) */
  TempFcrownWarn_Items DOUBLE PRECISION DEFAULT 0.0,                   /* The tube crown temperature warning setpoint for track (degF) */
  TempCcrownWarn_Items DOUBLE PRECISION DEFAULT 0.0,                   /* The tube crown temperature warning setpoint for track (degC) */
  TempFcrownWarnPreheat_Items DOUBLE PRECISION DEFAULT 0.0,            /* The tube crown preheat temperature warning setpoint for preheat(degF) */
  TempCcrownWarnPreheat_Items DOUBLE PRECISION DEFAULT 0.0,            /* The tube crown preheat temperature warning setpoint for preheat(degC) */
  TempFcrownTrip_Items DOUBLE PRECISION DEFAULT 0.0,                   /* The tube crown temperature trip alarm setpoint for track or preheat (deg F) */
  TempCcrownTrip_Items DOUBLE PRECISION DEFAULT 0.0,                   /* The tube crown temperature trip alarm setpoint for track or preheat (deg C) */
  TempFavgWallWarn_Items DOUBLE PRECISION DEFAULT 0.0,                 /* The tube average wall temperature warning setpoint for track or preheat (degF) */
  TempCavgWallWarn_Items DOUBLE PRECISION DEFAULT 0.0,                 /* The tube average wall temperature warning setpoint for track or preheat (degC) */
  TempFavgWallTrip_Items DOUBLE PRECISION DEFAULT 0.0,                 /* The tube average wall temperature trip setpoint for track or preheat (degF) */
  TempCavgWallTrip_Items DOUBLE PRECISION DEFAULT 0.0,                 /* The tube average wall temperature trip setpoint for track or preheat (degC) */
  PercentTubeStrainTrip_Items DOUBLE PRECISION DEFAULT 0.0,            /* The tube strain trip setpoint (percent) */
  PercentTubeStrainWarn_Items DOUBLE PRECISION DEFAULT 0.0,            /* The tube strain warning setpoint (percent) */
  dTempFTripDeadband_Items DOUBLE PRECISION DEFAULT 0.0,               /* The temperature trip deadband (degF) */
  dWFlowModelDeadband_Items DOUBLE PRECISION DEFAULT 0.0,              /* The Flow model deadband (lbm/s) */
  TempInnerWallWarn_Items DOUBLE PRECISION DEFAULT 0.0,                /* The tube inner wall temperature warning setpoint for track or preheat (System Units) */
  TempFinnerWallWarn_Items DOUBLE PRECISION DEFAULT 0.0,               /* The tube inner wall temperature warning setpoint for track or preheat (degF) */
  TempCinnerWallWarn_Items DOUBLE PRECISION DEFAULT 0.0,               /* The tube inner wall temperature warning setpoint for track or preheat (degC) */
  TempInnerWallTrip_Items DOUBLE PRECISION DEFAULT 0.0,                /* The tube inner wall temperature trip setpoint for track or preheat (System Units) */
  TempFinnerWallTrip_Items DOUBLE PRECISION DEFAULT 0.0,               /* The tube inner wall temperature trip setpoint for track or preheat (degF) */
  TempCinnerWallTrip_Items DOUBLE PRECISION DEFAULT 0.0,               /* The tube inner wall temperature trip setpoint for track or preheat (degC) */
  TempIRCameraWarn_Items DOUBLE PRECISION DEFAULT 0.0,                 /* The IR camera temperature warning setpoint for track or preheat (System Units) */
  TempFIRCameraWarn_Items DOUBLE PRECISION DEFAULT 0.0,                /* The IR camera temperature warning setpoint for track or preheat (degF) */
  TempCIRCameraWarn_Items DOUBLE PRECISION DEFAULT 0.0,                /* The IR camera temperature warning setpoint for track or preheat (degC) */
  TempIRCameraTrip_Items DOUBLE PRECISION DEFAULT 0.0,                 /* The IR camera temperature trip setpoint for track or preheat (System Units) */
  TempFIRCameraTrip_Items DOUBLE PRECISION DEFAULT 0.0,                /* The IR camera temperature trip setpoint for track or preheat (degF) */
  TempCIRCameraTrip_Items DOUBLE PRECISION DEFAULT 0.0,                /* The IR camera temperature trip setpoint for track or preheat (degC) */
  TempHeatShieldWarn_Items DOUBLE PRECISION DEFAULT 0.0,               /* The HeatShield temperature warning setpoint for track or preheat (System Units) */
  TempFHeatShieldWarn_Items DOUBLE PRECISION DEFAULT 0.0,              /* The HeatShield temperature warning setpoint for track or preheat (degF) */
  TempCHeatShieldWarn_Items DOUBLE PRECISION DEFAULT 0.0,              /* The HeatShield temperature warning setpoint for track or preheat (degC) */
  TempHeatShieldTrip_Items DOUBLE PRECISION DEFAULT 0.0,               /* The HeatShield temperature trip setpoint for track or preheat (System Units) */
  TempFHeatShieldTrip_Items DOUBLE PRECISION DEFAULT 0.0,              /* The HeatShield temperature trip setpoint for track or preheat (degF) */
  TempCHeatShieldTrip_Items DOUBLE PRECISION DEFAULT 0.0,              /* The HeatShield temperature trip setpoint for track or preheat (degC) */
  TempBackwallsWarn_Items DOUBLE PRECISION DEFAULT 0.0,                /* The Backwalls temperature warning setpoint for track (System Units) */
  TempFBackwallsWarn_Items DOUBLE PRECISION DEFAULT 0.0,               /* The Backwalls temperature warning setpoint for track (degF) */
  TempCBackwallsWarn_Items DOUBLE PRECISION DEFAULT 0.0,               /* The Backwalls temperature warning setpoint for track (degC) */
  TempBackwallsTrip_Items DOUBLE PRECISION DEFAULT 0.0,                /* The Backwalls temperature trip setpoint for track (System Units) */
  TempFBackwallsTrip_Items DOUBLE PRECISION DEFAULT 0.0,               /* The Backwalls temperature trip setpoint for track (degF) */
  TempCBackwallsTrip_Items DOUBLE PRECISION DEFAULT 0.0,               /* The Backwalls temperature trip setpoint for track (degC) */
  TempBackwallsWarnEmpty_Items DOUBLE PRECISION DEFAULT 0.0,           /* The Backwalls temperature warning setpoint when tubes are empty (System Units) */
  TempFBackwallsWarnEmpty_Items DOUBLE PRECISION DEFAULT 0.0,          /* The Backwalls temperature warning setpoint when tubes are empty (degF) */
  TempCBackwallsWarnEmpty_Items DOUBLE PRECISION DEFAULT 0.0,          /* The Backwalls temperature warning setpoint when tubes are empty (degC) */
  TempBackwallsTripEmpty_Items DOUBLE PRECISION DEFAULT 0.0,           /* The Backwalls temperature trip setpoint when tubes are empty (System Units) */
  TempFBackwallsTripEmpty_Items DOUBLE PRECISION DEFAULT 0.0,          /* The Backwalls temperature trip setpoint when tubes are empty (degF) */
  TempCBackwallsTripEmpty_Items DOUBLE PRECISION DEFAULT 0.0,          /* The Backwalls temperature trip setpoint when tubes are empty (degC) */
  FluxHeatShieldWarn_Items DOUBLE PRECISION DEFAULT 0.0,               /* The heatshield flux warning setpoint (kw/m^2) */
  FluxHeatShieldTrip_Items DOUBLE PRECISION DEFAULT 0.0,               /* The heatshield flux trip setpoint (kw/m^2) */
  TripOptions_Items INTEGER DEFAULT 0,                                 /* The UI Operator enables/disables trips */
    UseModelTrips_Items BOOLEAN DEFAULT false,                         /* Allow trip on model calculations */
    EnableInnerWallTrip_Items BOOLEAN DEFAULT false,                   /* Enable the Tube Innerwall temperature trip */
    EnableIRCamerasTrip_Items BOOLEAN DEFAULT false,                   /* Enable the IR Camera temperature trip */
    EnableBackWallsTrip_Items BOOLEAN DEFAULT false,                   /* Enable the Tube Backwall temperature trip */
    EnableBackWallsTripEmpty_Items BOOLEAN DEFAULT false,              /* Enable the Empty Tube Backwall temperature trip */
    EnableCircuit1Trip_Items BOOLEAN DEFAULT false,                    /* Enable the Circuit 1 Trip from the DCS */
    EnableCircuit2Trip_Items BOOLEAN DEFAULT false,                    /* Enable the Circuit 2 Trip from the DCS */
    EnablePushButtonTrip_Items BOOLEAN DEFAULT false,                  /* Enable the Pushbutton Trip from the DCS */
  TemperatureScale_Items INTEGER DEFAULT 0,                            /* The UI Operator can choose the lower or upper temperature scale for the IR temperatures */
    OperatorChoseLowerScale_Items BOOLEAN DEFAULT false,               /* Choose the Lower Temperature Scale */
    OperatorChoseHigherScale_Items BOOLEAN DEFAULT false,              /* Choose the Higher Temperature Scale */
    UseLowerTemperatureScale_Items BOOLEAN DEFAULT false,              /* The system is using the lower temperature scale */
    UseHigherTemperatureScale_Items BOOLEAN DEFAULT false,             /* The system is using the higher temperature scale */
  PreheatOptions_Items INTEGER DEFAULT 0,                              /* The UI Operator can issue the following preheat/postheat booster on/off commands */
    BoostersOn_Items BOOLEAN DEFAULT false,                            /* Enable the boosters (on/off) */
    BoostersUpper_Items BOOLEAN DEFAULT false,                         /* Enable the upper boosters (on/off) */
    BoostersLower_Items BOOLEAN DEFAULT false,                         /* Enable the lower boosters (on/off */
  TitleOperatorSavecase_Items VARCHAR(39) DEFAULT '',                  /* UI Operatore entered title of the Operator Savecase */
  QueueSCAPIEnd_Items INTEGER DEFAULT 0,                               /* The number of entries in the queue */
  QueueEphmrsEnd_Items INTEGER DEFAULT 0,                              /* The number of entries in the queue */
  QueueCamerasEnd_Items INTEGER DEFAULT 0,                             /* The number of entries in the queue */
  BalanceEnd_Items INTEGER DEFAULT 0,                                  /* The current balance entry (circular queue) */
  LogBalanceEnd_Items INTEGER DEFAULT 0,                               /* The current log balance entry (circular queue) */
  CommentForLog_Items VARCHAR(80) DEFAULT '',                          /* Operator comment for logging */
  DayDeltaUpdateIERS_Items INTEGER DEFAULT 0,                          /* The day delta to be used as the day of the month between IERS Updates */
  DayDeltaUpdatedIERS_Items INTEGER DEFAULT 0,                         /* The day delta last used as the day of the month between IERS Updates */
  A$Mess_Items VARCHAR(20) DEFAULT '',                                 /* MessLIB OPTION FIELD A */
  B$Mess_Items VARCHAR(20) DEFAULT '',                                 /* MessLIB OPTION FIELD B */
  C$Mess_Items BOOLEAN DEFAULT false,                                  /* MessLIB OPTION FIELD C */
  TimeTagAlarm_Items TIMESTAMPTZ DEFAULT now()                         /* Time tag when the log was last checked for alarms */
);
  
/* Log messages */
CREATE TABLE Mess(
  Instance_Mess INTEGER PRIMARY KEY,                                   /* Primary Key for the Mess Table */
  Time_Mess TIMESTAMPTZ DEFAULT now(),                                 /* Time when the Message was sent */
  Text_Mess VARCHAR(96) DEFAULT '',                                    /* Text of the Message */
  SEV_Mess VARCHAR(1) DEFAULT '',                                      /* Severity of the Message */
  COMPID_Mess VARCHAR(16) DEFAULT '',                                  /* Composite ID of the Message */
  REFERENCE$Message_Mess INTEGER DEFAULT 0                             /* The Message template associated with the message */
);
  
/* Log message templates */
CREATE TABLE Message(
  Instance_Message INTEGER PRIMARY KEY,                                /* Primary Key for the Message Table */
  ID_Message VARCHAR(10) DEFAULT '',                                   /* The ID of a message template */
  NameStatusCode_Message VARCHAR(20) DEFAULT '',                       /* The name of the status code associated with the message */
  NameMLFCode_Message VARCHAR(20) DEFAULT '',                          /* The name of the MLF status code associated with the message */
  Code_Message INTEGER DEFAULT 0,                                      /* The Code number of a message template */
  Severity_Message INTEGER DEFAULT 0,                                  /* The severity of the message status code */
    Warning_Message BOOLEAN DEFAULT false,                             /* The status code is a warning */
    Success_Message BOOLEAN DEFAULT false,                             /* The status code is normal */
    Error_Message BOOLEAN DEFAULT false,                               /* The status code is an error */
    Fatal_Message BOOLEAN DEFAULT false,                               /* The status code is a fatal error */
  Sev_Message VARCHAR(1) DEFAULT '',                                   /* Message Severity */
  Preface_Message VARCHAR(48) DEFAULT '',                              /* The Message Preface */
  Device_Message VARCHAR(16) DEFAULT '',                               /* The Device */
  Suffix_Message VARCHAR(48) DEFAULT '',                               /* The Message Suffix */
  LevelDetail_Message INTEGER DEFAULT 0,                               /* The explanation and action to be taken */
    ExplanationLine1_Message BOOLEAN DEFAULT false,                    /* Show Text of ExplanationLine1 */
    ExplanationLine2_Message BOOLEAN DEFAULT false,                    /* Show Text of ExplanationLine2 */
    ExplanationLine3_Message BOOLEAN DEFAULT false,                    /* Show Text of explanation3 */
    ExplanationLine4_Message BOOLEAN DEFAULT false,                    /* Show Text of ExplanationLine4 */
    ExplanationLine5_Message BOOLEAN DEFAULT false,                    /* Show Text of ExplanationLine5 */
    ExplanationLine6_Message BOOLEAN DEFAULT false,                    /* Show Text of ExplanationLine6 */
    ExplanationLine7_Message BOOLEAN DEFAULT false,                    /* Show Text of ExplanationLine7 */
    ExplanationLine8_Message BOOLEAN DEFAULT false,                    /* Show Text of ExplanationLine8 */
    ActionLine1_Message BOOLEAN DEFAULT false,                         /* Show Text of ActionLine1 */
    ActionLine2_Message BOOLEAN DEFAULT false,                         /* Show Text of ActionLine2 */
    ActionLine3_Message BOOLEAN DEFAULT false,                         /* Show Text of explanation3 */
    ActionLine4_Message BOOLEAN DEFAULT false,                         /* Show Text of ActionLine4 */
    ActionLine5_Message BOOLEAN DEFAULT false,                         /* Show Text of ActionLine5 */
    ActionLine6_Message BOOLEAN DEFAULT false,                         /* Show Text of ActionLine6 */
    ActionLine7_Message BOOLEAN DEFAULT false,                         /* Show Text of ActionLine7 */
    ActionLine8_Message BOOLEAN DEFAULT false,                         /* Show Text of ActionLine8 */
    Alarm_Message BOOLEAN DEFAULT false,                               /* The event that caused the message is an alarm */
  Explanation1_Message VARCHAR(56) DEFAULT '',                         /* Explanation of the event that caused the message */
  Explanation2_Message VARCHAR(56) DEFAULT '',                         /* Explanation of the event that caused the message */
  Explanation3_Message VARCHAR(56) DEFAULT '',                         /* Explanation of the event that caused the message */
  Explanation4_Message VARCHAR(56) DEFAULT '',                         /* Explanation of the event that caused the message */
  Explanation5_Message VARCHAR(56) DEFAULT '',                         /* Explanation of the event that caused the message */
  Explanation6_Message VARCHAR(56) DEFAULT '',                         /* Explanation of the event that caused the message */
  Explanation7_Message VARCHAR(56) DEFAULT '',                         /* Explanation of the event that caused the message */
  Explanation8_Message VARCHAR(56) DEFAULT '',                         /* Explanation of the event that caused the message */
  Action1_Message VARCHAR(56) DEFAULT '',                              /* Action to be taken if the event occurs */
  Action3_Message VARCHAR(56) DEFAULT '',                              /* Action to be taken if the event occurs */
  Action4_Message VARCHAR(56) DEFAULT '',                              /* Action to be taken if the event occurs */
  Action5_Message VARCHAR(56) DEFAULT '',                              /* Action to be taken if the event occurs */
  Action6_Message VARCHAR(56) DEFAULT '',                              /* Action to be taken if the event occurs */
  Action7_Message VARCHAR(56) DEFAULT '',                              /* Action to be taken if the event occurs */
  Action8_Message VARCHAR(56) DEFAULT '',                              /* Action to be taken if the event occurs */
  Action9_Message VARCHAR(56) DEFAULT '',                              /* Action to be taken if the event occurs */
  ActionA_Message VARCHAR(56) DEFAULT ''                               /* Action to be taken if the event occurs */
);
  
