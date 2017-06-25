"use strict";

exports.dateTimeFormatImpl = function(locales) {
  return function(options) {
    return new Intl.DateTimeFormat(locales, options);
  };
};

exports.createDateTimeFormatImpl = function(throwError, pure, locales, opts) {
  try {
    return pure(Intl.DateTimeFormat(locales, opts));
  } catch (e) {
    return throwError(e.message);
  }
};

exports.formatJSDate = function(formatter) {
  return function(date) {
    return formatter.format(date)
  };
};

exports.formatToPartsImpl = function(formatter) {
  return function(date) {
    return formatter.formatToParts(date);
  };
};

exports.supportedLocalesOfImpl = function(Left, Right, locales) {
  try {
    return Right(Intl.DateTimeFormat.supportedLocalesOf(locales))
  } catch (e) {
    return Left(e.message)
  }
}

exports.resolvedOptionsImpl = function(formatter) {
  return formatter.resolvedOptions();
};
