import {mulSafe, divSafe, isNumber, round} from "./utils.js";

export const fields = {};

const NumberField = fields.NumberField = {

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

export let Field = NumberField;

try {
    const D = Decimal;
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

      toSignificantDigits(digits) {
        return new DecimalFraction(this.toDecimal().toSignificantDigits(digits), new Decimal(1));
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

    function fr(n) {
        if(!(n instanceof DecimalFraction)) {
            return new DecimalFraction(n,1);
        }
        return n;
    }

    const DecimalOne = new DecimalFraction(1,1);
    const DecimalZero = new DecimalFraction(0,1);
    const DecimalField = fields.DecimalField = {

      isMember: (n) => {
        return n instanceof DecimalFraction || n instanceof Decimal;
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
        return fr(a).plus(fr(b));
      },

      sub: (a,b) => {
        return fr(a).minus(fr(b));
      },

      mul: function() {
        let result = DecimalOne;
        for (var i = 0; i < arguments.length; i++) {
          result = result.times(fr(arguments[i]));
        }
        return result;
      },

      div: (a,b) => {
        return fr(a).dividedBy(fr(b));
      },

      inverse: (n) => {
        return fr(n).inverse();
      },

      isExactlyZero: (n) => {
        return fr(n).isZero();
      },

      round: (n) => {
        return fr(n).round();
      },

      roundTo: (n,decimals) => {
        return fr(n).toDecimalPlaces(decimals);
      },

      lt: (a,b) => {
        return fr(a).lessThan(fr(b));
      },

      gt: (a,b) => {
        return fr(a).greaterThan(fr(b));
      },

      eq: (a,b) => {
        return fr(a).equals(fr(b));
      },

      pow: (a,b) => {
        return fr(a).toPower(fr(b));
      },

      abs: (n) => {
        return fr(n).abs();
      },

      PI: new DecimalFraction(Decimal.acos(-1),1),
    };
    DecimalField.divSafe = DecimalField.div;
    DecimalField.mulSafe = DecimalField.mul;

    Field = DecimalField;
} catch(e) {
}
