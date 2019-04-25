import {mulSafe, divSafe, isNumber, round} from "./utils.js";
import { Decimal } from "../../lib/decimal.js";

//window.Decimal = Decimal;

export const NumberField = {

  isMember: (n) => isNumber(n),

  fromString: (s) => parseFloat(s),

  fromNumber: (n) => n,

  toNumber: (n) => n,

  one: () => 1,

  zero: () => 0,

  add: (a,b) => a + b,

  sub: (a,b) => a - b,

  mul: (a,b) => a * b,

  mulSafe: function() {
    return mulSafe.apply(this,arguments);
  },

  div: (a,b) => a / b,

  divSafe: divSafe,

  inverse: (n) => 1 / n,

  isExactlyZero: (n) => n === 0,

  round: Math.round,

  roundTo: round,

  lt: (a,b) => a < b,

  gt: (a,b) => a > b,

  eq: (a,b) => a === b,

  pow: Math.pow,

  PI: Math.PI,

  abs: Math.abs
};

class DecimalFraction {
  constructor(n,d) {
    if (!Decimal.isDecimal(n)) {
      n = new Decimal(n);
    }
    if (!Decimal.isDecimal(d)) {
      d = new Decimal(d);
    }
    this.n = n;
    this.d = d;
  }

  toString() {
    return this.toDecimal().toString();
  }

  toDecimal() {
    if (this.d.eq(1)) {
      return this.n;
    }
    else {
      return this.n.div(this.d);
    }
  }

  plus(b) {
    return new DecimalFraction(this.n.mul(b.d).add(b.n.mul(this.d)),this.d.mul(b.d));
  }

  minus(b) {
    return new DecimalFraction(this.n.mul(b.d).sub(b.n.mul(this.d)),this.d.mul(b.d));
  }

  times(b) {
    return new DecimalFraction(this.n.mul(b.n), this.d.mul(b.d));
  }

  dividedBy(b) {
    return new DecimalFraction(this.n.mul(b.d), this.d.mul(b.n));
  }

  inverse() {
    return new DecimalFraction(this.d,this.n);
  }

  isZero() {
    return this.n.isZero();
  }

  round() {
    return new DecimalFraction(this.toDecimal().round(), new Decimal(1));
  }

  toDecimalPlaces(decimals) {
    return new DecimalFraction(this.toDecimal().toDecimalPlaces(decimals), new Decimal(1));
  }

  lessThan(b) {
    return this.toDecimal().lessThan(b.toDecimal());
  }

  greaterThan(b) {
    return this.toDecimal().greaterThan(b.toDecimal());
  }

  equals(b) {
    return this.toDecimal().equals(b.toDecimal());
  }

  toPower(b) {
    return new DecimalFraction(this.n.toPower(b.toDecimal()), this.d.toPower(b.toDecimal()));
  }

  abs() {
    return new DecimalFraction(this.n.absoluteValue(), this.d.absoluteValue());
  }
}

const DecimalOne = new DecimalFraction(1,1);
const DecimalZero = new DecimalFraction(0,1);
export const DecimalField = {

  isMember: (n) => {
    return n instanceof DecimalFraction;
  },

  fromString: (s) => {
    return new DecimalFraction(new Decimal(s), 1);
  },

  fromNumber: (n) => {
    return (new DecimalFraction(n,1)).toDecimalPlaces(12);
  },

  toNumber: (n) => {
    return n.toDecimal().toNumber();
  },

  one: () => {
    return DecimalOne;
  },

  zero: () => {
    return DecimalZero;
  },

  add: (a,b) => {
    return a.plus(b);
  },

  sub: (a,b) => {
    return a.minus(b);
  },

  mul: function() {
    let result = DecimalOne;
    for (var i = 0; i < arguments.length; i++) {
      result = result.times(arguments[i]);
    }
    return result;
  },

  div: (a,b) => {
    return a.dividedBy(b);
  },

  inverse: (n) => {
    return n.inverse();
  },

  isExactlyZero: (n) => {
    return n.isZero();
  },

  round: (n) => {
    return n.round();
  },

  roundTo: (n,decimals) => {
    return n.toDecimalPlaces(decimals);
  },

  lt: (a,b) => {
    return a.lessThan(b);
  },

  gt: (a,b) => {
    return a.greaterThan(b);
  },

  eq: (a,b) => {
    return a.equals(b);
  },

  pow: (a,b) => {
    return a.toPower(b);
  },

  abs: (n) => {
    return n.abs();
  },

  PI: new DecimalFraction(Decimal.acos(-1),1),
};
DecimalField.divSafe = DecimalField.div;
DecimalField.mulSafe = DecimalField.mul;

export let Field = DecimalField;
