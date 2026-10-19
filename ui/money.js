'use strict';

// No build system or API-model dependency. Monetary values and weights are BigInt;
// mode strings are the API literals equal/shares/percentage/exact. UUID identity
// and tie ordering are case insensitive. Allocation results retain input order.
(() => {
  const maxRecordMinor = 9223372036854775807n;
  const uuid = /^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i;
  const decimal = /^([0-9]+)(?:\.([0-9]+))?$/;
  const fail = (message) => { throw new Error(message); };
  const bigint = (value) => {
    if (typeof value !== 'bigint') fail('Money and split values must be BigInt');
    return value;
  };
  const checkDigits = (digits) => {
    if (!Number.isInteger(digits) || digits < 0 || digits > 4) {
      fail('minorDigits must be between 0 and 4');
    }
  };
  const scale = (digits) => 10n ** BigInt(digits);
  const checkRecordAmount = (amount, allowZero = false) => {
    bigint(amount);
    if (amount < 0n || (!allowZero && amount === 0n)) {
      fail(allowZero ? 'Amount must not be negative' : 'Amount must be greater than zero');
    }
    if (amount > maxRecordMinor) fail('Amount exceeds the maximum record amount');
  };

  // Only canonical wire strings; aggregates may exceed Int64. No JSON numbers.
  function parseMinorAmount(value, { allowNegative = false } = {}) {
    if (typeof value !== 'string' ||
        (allowNegative ? /^(0|-?[1-9][0-9]*)$/ : /^(0|[1-9][0-9]*)$/).exec(value)?.[0] !== value) {
      fail('Amount must be a canonical integer string');
    }
    return BigInt(value);
  }
  function parseDecimal(text, digits) {
    if (typeof text !== 'string') fail('Amount must be a decimal string');
    const match = decimal.exec(text.trim());
    if (!match) fail('Use an ordinary nonnegative decimal amount');
    const fraction = match[2] || '';
    if (fraction.length > digits) fail(`Amount allows at most ${digits} decimal places`);
    return BigInt(match[1]) * scale(digits) + BigInt(fraction.padEnd(digits, '0') || '0');
  }
  function parseMajorAmount(text, { minorDigits, allowZero = false }) {
    checkDigits(minorDigits);
    const amount = parseDecimal(text, minorDigits);
    checkRecordAmount(amount, allowZero);
    return amount;
  }
  function formatMajorAmount(amountMinor, { minorDigits }) {
    checkDigits(minorDigits);
    bigint(amountMinor);
    const sign = amountMinor < 0n ? '-' : '';
    const digits = (amountMinor < 0n ? -amountMinor : amountMinor).toString().padStart(minorDigits + 1, '0');
    if (minorDigits === 0) return sign + digits;
    const point = digits.length - minorDigits;
    return `${sign}${digits.slice(0, point)}.${digits.slice(point)}`;
  }
  function formatMoney({ code, minorDigits, amountMinor }) {
    return `${code} ${formatMajorAmount(amountMinor, { minorDigits })}`;
  }
  function parsePercentage(text) {
    const value = parseDecimal(text, 2);
    if (value > 10000n) fail('Percentage must be between 0 and 100');
    return value;
  }
  function checkRows(rows) {
    if (!Array.isArray(rows) || rows.length === 0) fail('Expense must include at least one split');
    const users = new Set();
    for (const row of rows) {
      if (typeof row.userId !== 'string' || row.userId.length !== 36 || !uuid.test(row.userId)) fail('Split user must be a UUID');
      const key = row.userId.toLowerCase();
      if (users.has(key)) fail('Each user may appear only once in splits');
      users.add(key);
      if (bigint(row.value) < 0n) fail('Split values must not be negative');
    }
  }
  function validateSplits(mode, amountMinor, rows) {
    checkRecordAmount(amountMinor);
    checkRows(rows);
    const total = rows.reduce((sum, row) => sum + row.value, 0n);
    switch (mode) {
      case 'equal':
        if (rows.some((row) => row.value !== 1n)) fail('Equal splits must have value 1');
        break;
      case 'shares':
        if (rows.some((row) => row.value > 32767n)) fail('Split shares must be between 0 and 32767');
        if (total === 0n) fail('Total split shares must be greater than zero');
        break;
      case 'percentage':
        if (rows.some((row) => row.value > 10000n) || total !== 10000n) {
          fail('Split percentages must total exactly 100%');
        }
        break;
      case 'exact':
        if (total !== amountMinor) fail('Exact splits must total the expense amount');
        break;
      default:
        fail('Unknown split method');
    }
  }
  // Displayable validation error or null; never normalizes the caller's inputs.
  function validateMoneySplits(mode, amountMinor, rows) {
    try { validateSplits(mode, amountMinor, rows); return null; }
    catch (error) { return error.message; }
  }
  // Generic weights have no Shares-specific upper bound, and aggregate totals
  // have no Int64 bound. Zero-weight rows never receive remainder units.
  function apportion(amountMinor, rows) {
    if (bigint(amountMinor) < 0n) fail('Allocation amount must not be negative');
    checkRows(rows);
    const total = rows.reduce((sum, row) => sum + row.value, 0n);
    if (total === 0n) fail('Total allocation weight must be positive');
    const allocated = rows.map((row) => amountMinor * row.value / total);
    const order = rows.map((row, index) => ({ index, key: row.userId.toLowerCase(), remainder: amountMinor * row.value % total }))
      .sort((a, b) => a.remainder !== b.remainder
        ? (a.remainder > b.remainder ? -1 : 1)
        : (a.key < b.key ? -1 : a.key > b.key ? 1 : 0));
    let remaining = amountMinor - allocated.reduce((sum, value) => sum + value, 0n);
    for (const { index } of order) {
      if (remaining === 0n) break;
      if (rows[index].value === 0n) continue;
      allocated[index] += 1n;
      remaining -= 1n;
    }
    return rows.map((row, index) => ({ userId: row.userId, value: allocated[index] }));
  }
  function allocateSplits(mode, amountMinor, rows) {
    validateSplits(mode, amountMinor, rows);
    return mode === 'exact' ? rows.map((row) => ({ ...row })) : apportion(amountMinor, rows);
  }
  // Produces the canonical wire text and an exact Rational numerator/denominator.
  function parseDecimalRate(text) {
    if (typeof text !== 'string') fail('Rate must be a decimal string');
    const match = decimal.exec(text.trim());
    if (!match) fail('Rate must be an ordinary positive decimal');
    const whole = BigInt(match[1]).toString();
    const fraction = (match[2] || '').replace(/0+$/, '');
    if (whole.length > 18 || fraction.length > 18) fail('Rate allows at most 18 integer and 18 decimal digits');
    const numerator = BigInt(whole + fraction);
    if (numerator === 0n) fail('Rate must be greater than zero');
    return Object.freeze({ text: fraction ? `${whole}.${fraction}` : whole, numerator, denominator: scale(fraction.length) });
  }
  // Direction: 1 source major unit = rate * target major units; half-up rounding.
  function convertMinorAmount(originalMinor, { sourceMinorDigits, targetMinorDigits, rate }) {
    checkDigits(sourceMinorDigits);
    checkDigits(targetMinorDigits);
    checkRecordAmount(originalMinor);
    if (bigint(rate.numerator) <= 0n || bigint(rate.denominator) <= 0n) fail('Rate must be greater than zero');
    const numerator = originalMinor * rate.numerator * scale(targetMinorDigits);
    const denominator = rate.denominator * scale(sourceMinorDigits);
    const converted = (numerator * 2n + denominator) / (denominator * 2n);
    if (converted === 0n) fail('Converted amount is below one minor unit');
    checkRecordAmount(converted);
    return converted;
  }
  // Original and settlement allocations use the SAME submitted logical ratios,
  // never the rounded original allocations (important for coarse JPY amounts).
  function allocateExpenseSplits(mode, originalMinor, rows, { settlementMinor }) {
    const original = allocateSplits(mode, originalMinor, rows);
    checkRecordAmount(settlementMinor);
    const settlement = apportion(settlementMinor, rows);
    return rows.map((row, index) => ({
      userId: row.userId, value: row.value,
      amountMinor: original[index].value,
      settlementAmountMinor: settlement[index].value,
    }));
  }

  globalThis.JizhangMoney = Object.freeze({
    maxRecordMinor, parseMinorAmount, parseMajorAmount, formatMajorAmount, formatMoney,
    parsePercentage, validateMoneySplits, apportion, allocateSplits,
    parseDecimalRate, convertMinorAmount, allocateExpenseSplits,
  });
})();
