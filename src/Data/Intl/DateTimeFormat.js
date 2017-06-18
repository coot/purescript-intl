"use strict";

exports["undefined"] = undefined;

exports.dateTimeFormatImpl = function(locales) {
  return function(options) {
    return new Intl.DateTimeFormat(locales, options);
  };
};

exports.createDateTimeFormatterImpl = function(locales) {
  return function(opts) {
    return Intl.DateTimeFormat(locales, opts);
  };
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
