module Syslog.Facility
  ( Facility(..)
  , facilityCode
  ) where

data Facility
  = FacilityKernelMessages
  | FacilityUserLevelMessages
  | FacilityMailSystem
  | FacilitySystemDaemons
  | FacilitySecurityAuthorizationMessages
  | FacilityMessagesGeneratedInternallyBySyslogd
  | FacilityLinePrinterSubsystem
  | FacilityNetworkNewsSubsystem
  | FacilityUUCPSubsystem
  | FacilityClockDaemon
  | FacilityFTPDaemon
  | FacilityNTPSubsystem
  | FacilityLogAudit
  | FacilityLogAlert
  | FacilityLocalUse0
  | FacilityLocalUse1
  | FacilityLocalUse2
  | FacilityLocalUse3
  | FacilityLocalUse4
  | FacilityLocalUse5
  | FacilityLocalUse6
  | FacilityLocalUse7

facilityCode :: Facility -> Int
facilityCode FacilityKernelMessages = 0
facilityCode FacilityUserLevelMessages = 1
facilityCode FacilityMailSystem = 2
facilityCode FacilitySystemDaemons = 3
facilityCode FacilitySecurityAuthorizationMessages = 4
facilityCode FacilityMessagesGeneratedInternallyBySyslogd = 5
facilityCode FacilityLinePrinterSubsystem = 6
facilityCode FacilityNetworkNewsSubsystem = 7
facilityCode FacilityUUCPSubsystem = 8
facilityCode FacilityClockDaemon = 9
facilityCode FacilityFTPDaemon = 11
facilityCode FacilityNTPSubsystem = 12
facilityCode FacilityLogAudit = 13
facilityCode FacilityLogAlert = 14
facilityCode FacilityLocalUse0 = 16
facilityCode FacilityLocalUse1 = 17
facilityCode FacilityLocalUse2 = 18
facilityCode FacilityLocalUse3 = 19
facilityCode FacilityLocalUse4 = 20
facilityCode FacilityLocalUse5 = 21
facilityCode FacilityLocalUse6 = 22
facilityCode FacilityLocalUse7 = 23
