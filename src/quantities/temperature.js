import Qty from "./constructor.js";
import { UNITY_ARRAY } from "./definitions.js";
import QtyError from "./error.js";
import { assign, compareArray } from "./utils.js";
import { Field } from "./fields.js";

assign(Qty.prototype, {
  isDegrees: function() {
    // signature may not have been calculated yet
    return (this.signature === null || this.signature === 400) &&
      this.numerator.length === 1 &&
      compareArray(this.denominator, UNITY_ARRAY) &&
      (this.numerator[0].match(/<temp-[CFRK]>/) || this.numerator[0].match(/<(kelvin|celsius|rankine|fahrenheit)>/));
  },

  isTemperature: function() {
    return this.isDegrees() && this.numerator[0].match(/<temp-[CFRK]>/);
  }
});

export function subtractTemperatures(lhs,rhs) {
  var lhsUnits = lhs.units();
  var rhsConverted = rhs.to(lhsUnits);
  var dstDegrees = Qty(getDegreeUnits(lhsUnits));
  return Qty({"scalar": Field.sub(lhs.scalar, rhsConverted.scalar), "numerator": dstDegrees.numerator, "denominator": dstDegrees.denominator});
}

export function subtractTempDegrees(temp,deg) {
  var tempDegrees = deg.to(getDegreeUnits(temp.units()));
  return Qty({"scalar": Field.sub(temp.scalar, tempDegrees.scalar), "numerator": temp.numerator, "denominator": temp.denominator});
}

export function addTempDegrees(temp,deg) {
  var tempDegrees = deg.to(getDegreeUnits(temp.units()));
  return Qty({"scalar": Field.add(temp.scalar, tempDegrees.scalar), "numerator": temp.numerator, "denominator": temp.denominator});
}

function getDegreeUnits(units) {
  if (units === "tempK") {
    return "degK";
  }
  else if (units === "tempC") {
    return "degC";
  }
  else if (units === "tempF") {
    return "degF";
  }
  else if (units === "tempR") {
    return "degR";
  }
  else {
    throw new QtyError("Unknown type for temp conversion from: " + units);
  }
}

var five = Field.fromNumber(5);
var nine = Field.fromNumber(9);
export function toDegrees(src,dst) {
  var srcDegK = toDegK(src);
  var dstUnits = dst.units();
  var dstScalar;

  if (dstUnits === "degK") {
    dstScalar = srcDegK.scalar;
  }
  else if (dstUnits === "degC") {
    dstScalar = srcDegK.scalar ;
  }
  else if (dstUnits === "degF") {
    dstScalar = Field.div(Field.mul(srcDegK.scalar, nine), five);
  }
  else if (dstUnits === "degR") {
    dstScalar = Field.dive(Field.mul(srcDegK.scalar, nine), five);
  }
  else {
    throw new QtyError("Unknown type for degree conversion to: " + dstUnits);
  }

  return Qty({"scalar": dstScalar, "numerator": dst.numerator, "denominator": dst.denominator});
}

function toDegK(qty) {
  var units = qty.units();
  var q;
  if (units.match(/(deg)[CFRK]/)) {
    q = qty.baseScalar;
  }
  else if (units === "tempK") {
    q = qty.scalar;
  }
  else if (units === "tempC") {
    q = qty.scalar;
  }
  else if (units === "tempF") {
    q = Field.div(Field.mul(qty.scalar, five),nine);
  }
  else if (units === "tempR") {
    q = Field.div(Field.mul(qty.scalar, five),nine);
  }
  else {
    throw new QtyError("Unknown type for temp conversion from: " + units);
  }

  return Qty({"scalar": q, "numerator": ["<kelvin>"], "denominator": UNITY_ARRAY});
}

export function toTemp(src,dst) {
  var dstUnits = dst.units();
  var dstScalar;

  if (dstUnits === "tempK") {
    dstScalar = src.baseScalar;
  }
  else if (dstUnits === "tempC") {
    dstScalar = Field.sub(src.baseScalar, Field.fromNumber(273.15));
  }
  else if (dstUnits === "tempF") {
    dstScalar = Field.sub(Field.div(Field.mul(src.baseScalar, nine), five), Field.fromNumber(459.67));
  }
  else if (dstUnits === "tempR") {
    dstScalar = Field.div(Field.mul(src.baseScalar, nine), five);
  }
  else {
    throw new QtyError("Unknown type for temp conversion to: " + dstUnits);
  }

  return Qty({"scalar": dstScalar, "numerator": dst.numerator, "denominator": dst.denominator});
}

export function toTempK(qty) {
  var units = qty.units();
  var q;
  if (units.match(/(deg)[CFRK]/)) {
    q = qty.baseScalar;
  }
  else if (units === "tempK") {
    q = qty.scalar;
  }
  else if (units === "tempC") {
    q = Field.add(qty.scalar, Field.fromNumber(273.15));
  }
  else if (units === "tempF") {
    q = Field.div(Field.mul(Field.add(qty.scalar, Field.fromNumber(459.67)), five), nine);
  }
  else if (units === "tempR") {
    q = Field.div(Field.mul(qty.scalar, five), nine);
  }
  else {
    throw new QtyError("Unknown type for temp conversion from: " + units);
  }

  return Qty({"scalar": q, "numerator": ["<temp-K>"], "denominator": UNITY_ARRAY});
}
