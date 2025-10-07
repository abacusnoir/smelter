(defpackage #:smelter.stdlib.datetime
  (:use #:coalton #:coalton-prelude)
  (:export
   ;; Core Types
   #:instant #:timezone #:zoneddatetime #:duration #:period #:dayofweek

   ;; Constructors
   #:now #:instant-of #:timezone-of #:system-timezone

   ;; Conversion
   #:to-zoned-datetime

   ;; Duration functions
   #:duration-seconds #:duration-minutes #:duration-hours

   ;; Period functions
   #:period-days #:period-months #:period-years))

(in-package #:smelter.stdlib.datetime)

(coalton-toplevel

  ;; Core Data Types

  (define-type Instant
    "Represents a single point in universal time as nanoseconds since Unix epoch."
    (Instant Integer))

  (define-type TimeZone
    "Represents a time zone with offset and daylight saving rules."
    (TimeZone String))  ; IANA timezone name like "America/New_York"

  (define-type ZonedDateTime
    "Represents a date and time in a specific timezone."
    (ZonedDateTime Instant TimeZone))

  (define-type Duration
    "Represents a duration of time as nanoseconds."
    (Duration Integer))

  (define-type Period
    "Represents a calendar-based duration (years, months, days)."
    (Period Integer Integer Integer))  ; years months days

  (define-type DayOfWeek
    "Represents a day of the week."
    Monday Tuesday Wednesday Thursday Friday Saturday Sunday)

  ;; Constructors and System Functions

  (declare now (Unit -> Instant))
  (define (now)
    "Get the current system time as an Instant."
    (lisp Instant ()
      (let ((universal-time (get-universal-time)))
        ;; Convert from Universal Time (1900-01-01) to Unix epoch (1970-01-01)
        ;; Universal time is seconds since 1900-01-01, Unix is since 1970-01-01
        ;; Difference is 70 years = 2208988800 seconds
        (let ((unix-seconds (- universal-time 2208988800)))
          (Instant (* unix-seconds 1000000000))))))  ; Convert to nanoseconds

  (declare instant-of (Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> (Optional Instant)))
  (define (instant-of year month day hour minute second nanosecond)
    "Create an Instant from UTC date/time parts. Returns None if invalid."
    (lisp (Optional Instant) (year month day hour minute second nanosecond)
      (handler-case
          (let ((universal-time (encode-universal-time second minute hour day month year 0)))
            (let ((unix-seconds (- universal-time 2208988800)))
              (coalton-prelude:Some (Instant (+ (* unix-seconds 1000000000) nanosecond)))))
        (error () coalton-prelude:None))))

  (declare system-timezone (Unit -> TimeZone))
  (define (system-timezone)
    "Get the system's default timezone."
    (TimeZone "UTC"))  ; Simplified for now

  (declare timezone-of (String -> (Optional TimeZone)))
  (define (timezone-of iana-name)
    "Create a TimeZone from an IANA timezone name (e.g., 'America/New_York')."
    (lisp (Optional TimeZone) (iana-name)
      (if (> (length iana-name) 0)
          (coalton-prelude:Some (TimeZone iana-name))
          coalton-prelude:None)))

  ;; Conversion Functions

  (declare to-zoned-datetime (Instant -> TimeZone -> ZonedDateTime))
  (define (to-zoned-datetime instant timezone)
    "Convert an Instant to a ZonedDateTime in the given timezone."
    (ZonedDateTime instant timezone))

  ;; Helper Functions for creating Durations and Periods

  (declare duration-seconds (Integer -> Duration))
  (define (duration-seconds seconds)
    "Create a Duration from seconds."
    (Duration (* seconds 1000000000)))

  (declare duration-minutes (Integer -> Duration))
  (define (duration-minutes minutes)
    "Create a Duration from minutes."
    (Duration (* (* minutes 60) 1000000000)))

  (declare duration-hours (Integer -> Duration))
  (define (duration-hours hours)
    "Create a Duration from hours."
    (Duration (* (* (* hours 60) 60) 1000000000)))

  (declare period-days (Integer -> Period))
  (define (period-days days)
    "Create a Period from days."
    (Period 0 0 days))

  (declare period-months (Integer -> Period))
  (define (period-months months)
    "Create a Period from months."
    (Period 0 months 0))

  (declare period-years (Integer -> Period))
  (define (period-years years)
    "Create a Period from years."
    (Period years 0 0)))