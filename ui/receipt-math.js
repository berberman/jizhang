'use strict';

(() => {
  const M = globalThis.JizhangMoney;
  const kinds = 'discount tax service_charge actual_tip rounding other';
  const treatments = 'add subtract included_in_line_total informational';
  const fail = (message) => { throw new Error(message); };
  const require = (condition, message) => { if (!condition) fail(message); };
  const matches = (pattern, value) => typeof value === 'string' && pattern.exec(value)?.[0] === value;
  const validReceiptItemId = (value) => matches(/^[a-z0-9_]{1,64}$/, value);

  function fields(value, names) {
    const keys = names.split(' ');
    require(value !== null && typeof value === 'object' && !Array.isArray(value) &&
      (Object.getPrototypeOf(value) === Object.prototype || Object.getPrototypeOf(value) === null) &&
      Reflect.ownKeys(value).length === keys.length && keys.every((key) => Object.hasOwn(value, key)),
    'Unexpected or missing receipt fields');
    for (const key of keys) require(Object.hasOwn(Object.getOwnPropertyDescriptor(value, key), 'value'), 'Receipt fields must be plain JSON values');
    return value;
  }
  function text(value, bound = 2048) {
    require(typeof value === 'string' && [...value].length <= bound && !value.includes('\0'), 'Receipt text exceeds its bound or contains NUL');
    return value;
  }
  const nullableText = (value, bound = 2048) => value === null ? null : text(value, bound);
  function choice(value, options) {
    require(options.split(' ').includes(text(value)), 'Unknown receipt enum');
    return value;
  }
  function list(value, parse, bound = 200) {
    require(Array.isArray(value) && value.length <= bound, 'Too many receipt entries or invalid array');
    const result = [];
    for (let index = 0; index < value.length; index++) {
      require(Object.hasOwn(value, index), 'Receipt arrays must not contain missing entries');
      result.push(parse(value[index]));
    }
    return Object.freeze(result);
  }
  function bounded(value) {
    require(new TextEncoder().encode(JSON.stringify(value)).byteLength <= 256 * 1024, 'Receipt evidence/review exceeds 256 KiB');
    return Object.freeze(value);
  }
  function id(value) {
    require(validReceiptItemId(text(value, 64)), 'Invalid receipt row ID');
    return value;
  }
  function minor(value) {
    M.parseMinorAmount(value);
    return value;
  }
  const nullableMinor = (value) => value === null ? null : minor(value);
  function observation(value) {
    const j = fields(value, 'value raw status');
    const result = {
      value: nullableText(j.value), raw: nullableText(j.raw),
      status: choice(j.status, 'visible absent unreadable ambiguous'),
    };
    require((result.value !== null) === (result.status === 'visible') &&
      (result.status !== 'visible' || result.raw !== null), 'Observation status does not match value/raw');
    return Object.freeze(result);
  }
  function evidenceCurrency(value) {
    const j = fields(value, 'code symbol basis evidence');
    return Object.freeze({code: nullableText(j.code), symbol: nullableText(j.symbol),
      basis: choice(j.basis, 'explicit contextual unknown'), evidence: nullableText(j.evidence)});
  }
  function evidenceItem(value) {
    const j = fields(value, 'id description raw_line quantity unit unit_price line_total price_basis modifiers tax_markers');
    return Object.freeze({id: text(j.id, 64), description: text(j.description, 512), raw_line: text(j.raw_line),
      quantity: observation(j.quantity), unit: observation(j.unit), unit_price: observation(j.unit_price),
      line_total: observation(j.line_total), price_basis: choice(j.price_basis, 'net gross unknown'),
      modifiers: list(j.modifiers, (v) => text(v, 512)), tax_markers: list(j.tax_markers, (v) => text(v, 512))});
  }
  function evidenceAdjustment(value) {
    const j = fields(value, 'id kind scope item_id amount treatment evidence');
    return Object.freeze({id: text(j.id, 64), kind: choice(j.kind, kinds), scope: choice(j.scope, 'item receipt unknown'),
      item_id: nullableText(j.item_id, 64), amount: observation(j.amount),
      treatment: choice(j.treatment, `${treatments} unknown`), evidence: text(j.evidence)});
  }
  function evidenceTotals(value) {
    const j = fields(value, 'subtotal tax_total printed_total final_paid');
    return Object.freeze({subtotal: observation(j.subtotal), tax_total: observation(j.tax_total),
      printed_total: observation(j.printed_total), final_paid: observation(j.final_paid)});
  }
  function suggestedTip(value) {
    const j = fields(value, 'rate amount');
    return Object.freeze({rate: observation(j.rate), amount: observation(j.amount)});
  }
  function modelIssue(value) {
    const j = fields(value, 'field reason');
    return Object.freeze({field: text(j.field, 512), reason: text(j.reason)});
  }
  function validateEvidence(value) {
    const j = fields(value, 'document_kind merchant transaction_date currency items adjustments totals suggested_tips issues');
    const merchant = observation(j.merchant);
    if (merchant.value !== null) text(merchant.value, 512);
    return bounded({document_kind: choice(j.document_kind, 'retail_receipt restaurant_receipt restaurant_bill fuel_receipt unknown'),
      merchant, transaction_date: observation(j.transaction_date), currency: evidenceCurrency(j.currency),
      items: list(j.items, evidenceItem), adjustments: list(j.adjustments, evidenceAdjustment),
      totals: evidenceTotals(j.totals), suggested_tips: list(j.suggested_tips, suggestedTip),
      issues: list(j.issues, modelIssue, 400)});
  }
  function reviewItem(value) {
    const j = fields(value, 'id title amountMinor quantity unit unitPrice');
    return Object.freeze({id: id(j.id), title: text(j.title, 512), amountMinor: minor(j.amountMinor),
      quantity: nullableText(j.quantity, 512), unit: nullableText(j.unit, 512), unitPrice: nullableText(j.unitPrice, 512)});
  }
  function reviewAdjustment(value) {
    const j = fields(value, 'id kind amountMinor treatment itemIds');
    return Object.freeze({id: id(j.id), kind: choice(j.kind, kinds), amountMinor: minor(j.amountMinor),
      treatment: choice(j.treatment, treatments), itemIds: list(j.itemIds, id)});
  }
  function validateReview(value) {
    const j = fields(value, 'mode merchant totalTitle currency date totalBasis printedTotalMinor finalPaidMinor totalMinor tipDecision actualTipMinor items adjustments acknowledgedWarnings confirmed');
    const currency = text(j.currency, 3);
    require(matches(/^[A-Z]{3}$/, currency), 'Invalid currency code');
    require(typeof j.confirmed === 'boolean', 'Expected a receipt boolean');
    return bounded({mode: choice(j.mode, 'itemized total'), merchant: text(j.merchant, 512), totalTitle: text(j.totalTitle, 512),
      currency, date: text(j.date, 10), totalBasis: choice(j.totalBasis, 'printed_total final_paid user_entered'),
      printedTotalMinor: nullableMinor(j.printedTotalMinor), finalPaidMinor: nullableMinor(j.finalPaidMinor),
      totalMinor: minor(j.totalMinor), tipDecision: choice(j.tipDecision, 'confirmed none bill_only'),
      actualTipMinor: nullableMinor(j.actualTipMinor), items: list(j.items, reviewItem), adjustments: list(j.adjustments, reviewAdjustment),
      acknowledgedWarnings: list(j.acknowledgedWarnings, (v) => text(v, 640), 1200), confirmed: j.confirmed});
  }
  function validReceiptDate(value) {
    if (!matches(/^[0-9]{4}-[0-9]{2}-[0-9]{2}$/, value)) return false;
    const year = Number(value.slice(0, 4));
    const month = Number(value.slice(5, 7));
    const day = Number(value.slice(8, 10));
    const leap = year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0);
    const days = [31, leap ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
    return month >= 1 && month <= 12 && day >= 1 && day <= days[month - 1];
  }
  function parseReceiptDecimal(value) {
    const match = typeof value === 'string' && /^([0-9]{1,18})(?:\.([0-9]{1,18}))?$/.exec(value);
    require(match && match[0] === value, 'Use an unsigned decimal with at most 18 integer and 18 fractional digits');
    const fraction = match[2] || '';
    return Object.freeze({numerator: BigInt(match[1] + fraction), denominator: 10n ** BigInt(fraction.length)});
  }
  function currencyInfo(catalog, code) {
    const info = catalog.find((entry) => entry.code === code);
    require(info, 'Confirm a supported catalog currency');
    return info;
  }
  function evidenceMoney(info, value) {
    require(value === value.trim(), 'Money must not contain surrounding whitespace');
    return M.parseMajorAmount(value, {minorDigits: info.minorUnits, allowZero: true});
  }
  function valid(action) {
    try { action(); return true; } catch (_) { return false; }
  }
  const invalidTreatment = (kind, treatment) => (kind === 'discount' && treatment === 'add') || (kind === 'actual_tip' && treatment === 'subtract');
  function roundedProduct(info, quantity, price) {
    const q = parseReceiptDecimal(quantity);
    const p = parseReceiptDecimal(price);
    const numerator = q.numerator * p.numerator * 10n ** BigInt(info.minorUnits);
    const denominator = q.denominator * p.denominator;
    return (numerator * 2n + denominator) / (denominator * 2n);
  }
  const issue = (severity, code, field, message) => Object.freeze({code, field, message, severity});
  const mismatch = (field) => issue('warning', 'quantity_price_mismatch', field, 'Quantity times unit price differs from the printed extended amount; use the confirmed extended amount');
  const warningToken = (warning) => warning.code.startsWith('model_issue_') ? warning.code : `${warning.code}:${warning.field}`;
  const unique = (values) => new Set(values).size === values.length;

  function evidenceIssues(catalog, evidence) {
    const result = [];
    let info;
    try { info = currencyInfo(catalog, evidence.currency.code); }
    catch (_) { result.push(issue('error', 'currency_required', 'currency', 'Confirm a supported catalog currency')); }
    if (evidence.currency.basis !== 'explicit') result.push(issue('warning', 'currency_unconfirmed', 'currency', 'Currency was inferred or is unknown; confirm it explicitly'));
    if (!validReceiptDate(evidence.transaction_date.value)) result.push(issue('error', 'date_required', 'transaction_date', 'Confirm an explicit full YYYY-MM-DD date; no year is inferred'));
    const ids = evidence.items.map((item) => item.id);
    const adjustmentIds = evidence.adjustments.map((adjustment) => adjustment.id);
    if (!unique(ids) || !ids.every(validReceiptItemId)) result.push(issue('error', 'item_ids', 'items', 'Item IDs must be unique lowercase ASCII identifiers'));
    if (!unique(adjustmentIds) || !adjustmentIds.every(validReceiptItemId)) result.push(issue('error', 'adjustment_ids', 'adjustments', 'Adjustment IDs must be unique lowercase ASCII identifiers'));
    const decimal = (field, value, severity) => valid(() => parseReceiptDecimal(value)) ? [] :
      [issue(severity, 'invalid_decimal', field, 'Enter an ordinary unsigned decimal with at most 18 integer and 18 fractional digits')];
    const optionalDecimal = (field, observation) => observation.value === null ? [] : decimal(field, observation.value, 'warning');
    const money = (required, field, observation) => {
      if (observation.value === null) return required ? [issue('error', 'amount_required', field, 'Enter the printed amount or correct this field')] : [];
      if (!info) return decimal(field, observation.value, 'error');
      return valid(() => evidenceMoney(info, observation.value)) ? [] : [issue('error', 'invalid_amount', field, 'Amount is not a valid nonnegative amount at the selected currency precision')];
    };
    for (const item of evidence.items) {
      const field = `items.${item.id}`;
      result.push(...money(true, `${field}.line_total`, item.line_total), ...optionalDecimal(`${field}.quantity`, item.quantity), ...optionalDecimal(`${field}.unit_price`, item.unit_price));
      if (info && item.quantity.value !== null && item.unit_price.value !== null && item.line_total.value !== null) {
        try {
          if (roundedProduct(info, item.quantity.value, item.unit_price.value) !== evidenceMoney(info, item.line_total.value)) result.push(mismatch(field));
        } catch (_) { /* Invalid observations already have field issues. */ }
      }
    }
    for (const adjustment of evidence.adjustments) {
      const field = `adjustments.${adjustment.id}`;
      result.push(...money(true, `${field}.amount`, adjustment.amount));
      if (adjustment.treatment === 'unknown') result.push(issue('error', 'treatment_required', `${field}.treatment`, 'Confirm how this adjustment affects the extracted item amounts'));
      if (adjustment.item_id !== null && !ids.includes(adjustment.item_id)) result.push(issue('error', 'item_reference', `${field}.item_id`, 'Adjustment references an unknown item'));
      if (adjustment.scope === 'item' && adjustment.item_id === null) result.push(issue('error', 'item_reference', `${field}.item_id`, 'An item-scoped adjustment must identify an item'));
      if (invalidTreatment(adjustment.kind, adjustment.treatment)) result.push(issue('error', 'invalid_treatment', `${field}.treatment`, 'Discounts cannot add and actual tips cannot subtract'));
    }
    for (const field of ['subtotal', 'tax_total', 'printed_total', 'final_paid']) result.push(...money(false, `totals.${field}`, evidence.totals[field]));
    evidence.suggested_tips.forEach((tip, index) => {
      for (const warning of [...optionalDecimal(`suggested_tips.${index}.rate`, tip.rate), ...money(false, `suggested_tips.${index}.amount`, tip.amount)]) {
        result.push(issue('warning', warning.code, warning.field, warning.message));
      }
    });
    evidence.issues.forEach((value, index) => result.push(issue('warning', `model_issue_${index}`, value.field, value.reason)));
    return result;
  }
  function receiptEvidenceReviewIssues(catalog, evidence) {
    return Object.freeze(evidenceIssues(catalog, validateEvidence(evidence)));
  }
  function validateReceiptEvidence(catalog, evidence) {
    return Object.freeze(evidenceIssues(catalog, validateEvidence(evidence)).slice(0, 400));
  }
  function receiptReviewNumericWarnings(catalog, review) {
    const info = currencyInfo(catalog, review.currency);
    const result = [];
    for (const item of review.items) {
      if (item.quantity !== null && item.unitPrice !== null && valid(() => { parseReceiptDecimal(item.quantity); parseReceiptDecimal(item.unitPrice); }) &&
        roundedProduct(info, item.quantity, item.unitPrice) !== M.parseMinorAmount(item.amountMinor)) result.push(mismatch(`items.${item.id}`));
    }
    return Object.freeze(result);
  }
  function acknowledge(review, warnings) {
    for (const warning of warnings) {
      const token = warningToken(warning);
      require(review.acknowledgedWarnings.includes(token), `Acknowledge warning ${token}`);
    }
  }
  function validateTip(review) {
    const tips = review.adjustments.filter((adjustment) => adjustment.kind === 'actual_tip');
    const tip = review.actualTipMinor === null ? null : BigInt(review.actualTipMinor);
    switch (review.tipDecision) {
      case 'none':
        require(tip === 0n, 'No tip requires actualTipMinor=0');
        require(!tips.some((adjustment) => BigInt(adjustment.amountMinor) !== 0n), 'No tip forbids nonzero actual-tip adjustments');
        break;
      case 'confirmed':
        require(tip !== null, 'Confirm the actual tip amount, already included in the total');
        require(tip <= BigInt(review.totalMinor), 'Actual tip exceeds the confirmed total');
        if (review.mode === 'itemized') require(tip === tips.reduce((sum, adjustment) => sum + BigInt(adjustment.amountMinor), 0n), 'Actual tip must equal the canonical actual-tip adjustments represented once');
        break;
      case 'bill_only':
        require(tip === null && review.finalPaidMinor === null, 'Bill-only review must leave actual tip and final payment unknown');
        require(review.totalBasis !== 'final_paid', 'Bill-only review cannot use final paid as its total basis');
        require(!tips.some((adjustment) => adjustment.treatment === 'add'), 'Bill-only review cannot add an actual tip');
        require(review.acknowledgedWarnings.includes('bill_only:tipDecision'), 'Confirm: Record bill amount only; actual tip is unknown');
        break;
    }
  }
  function recordAmount(value) {
    require(value > 0n && value <= M.maxRecordMinor, 'Amount must be positive and within the supported record range');
  }
  function computeReceiptImportAmounts(catalog, value) {
    const review = validateReview(value);
    currencyInfo(catalog, review.currency);
    require(review.confirmed, 'Confirm the receipt review');
    require(validReceiptDate(review.date), 'Confirm an explicit full YYYY-MM-DD date');
    const total = BigInt(review.totalMinor);
    require(total > 0n, 'Confirmed total must be positive');
    if (review.totalBasis === 'printed_total') require(review.printedTotalMinor === review.totalMinor, 'Printed total must equal the confirmed total');
    if (review.totalBasis === 'final_paid') require(review.finalPaidMinor === review.totalMinor, 'Final paid amount must equal the confirmed total');
    validateTip(review);
    if (review.mode === 'total') {
      require(review.items.length === 0 && review.adjustments.length === 0, 'Total mode must retain empty item and adjustment lists');
      require(review.totalTitle.trim().length > 0, 'Enter a total expense title');
      recordAmount(total);
      return Object.freeze([Object.freeze({itemId: 'total', title: review.totalTitle, amountMinor: total})]);
    }
    const keys = review.items.map((item) => item.id);
    require(keys.length > 0, 'Itemized mode requires at least one item');
    require(unique(keys), 'Item IDs must be unique');
    require(unique(review.adjustments.map((adjustment) => adjustment.id)), 'Adjustment IDs must be unique');
    for (const item of review.items) {
      require(item.title.trim().length > 0, `Enter a title for item ${item.id}`);
      if (item.quantity !== null) parseReceiptDecimal(item.quantity);
      if (item.unitPrice !== null) parseReceiptDecimal(item.unitPrice);
    }
    for (const adjustment of review.adjustments) {
      require(!invalidTreatment(adjustment.kind, adjustment.treatment), 'Discounts cannot add and actual tips cannot subtract');
      require(unique(adjustment.itemIds) && adjustment.itemIds.every((key) => keys.includes(key)), 'Adjustment eligibility must contain unique existing item IDs');
      if (adjustment.treatment === 'add' || adjustment.treatment === 'subtract') require(adjustment.itemIds.length > 0, 'Confirm a nonempty eligible item set for every external adjustment');
    }
    acknowledge(review, receiptReviewNumericWarnings(catalog, review));
    const bases = new Map(review.items.map((item) => [item.id, BigInt(item.amountMinor)]));
    function weights(adjustment) {
      const rows = keys.filter((key) => adjustment.itemIds.includes(key)).map((key) => ({key, value: bases.get(key)}));
      require(rows.length > 0 && rows.reduce((sum, row) => sum + row.value, 0n) > 0n, 'Adjustment requires positive eligible item bases');
      return rows;
    }
    for (const adjustment of review.adjustments.filter((row) => row.treatment === 'subtract')) {
      const rows = weights(adjustment);
      const amount = BigInt(adjustment.amountMinor);
      require(amount <= rows.reduce((sum, row) => sum + row.value, 0n), 'Discount exceeds eligible item bases');
      for (const row of M.apportionByKey(amount, rows)) bases.set(row.key, bases.get(row.key) - row.value);
    }
    // Added fees share discounted bases, never a previous fee's allocations.
    const finalAmounts = new Map(bases);
    for (const adjustment of review.adjustments.filter((row) => row.treatment === 'add')) {
      for (const row of M.apportionByKey(BigInt(adjustment.amountMinor), weights(adjustment))) finalAmounts.set(row.key, finalAmounts.get(row.key) + row.value);
    }
    const result = review.items.filter((item) => finalAmounts.get(item.id) !== 0n)
      .map((item) => Object.freeze({itemId: item.id, title: item.title, amountMinor: finalAmounts.get(item.id)}));
    for (const amount of result) recordAmount(amount.amountMinor);
    require(result.reduce((sum, row) => sum + row.amountMinor, 0n) === total, 'Itemized amounts must exactly equal the confirmed total');
    return Object.freeze(result);
  }
  function receiptImportEvidenceWarnings(catalog, evidence, mode) {
    if (evidence === null) return [];
    return evidenceIssues(catalog, validateEvidence(evidence)).filter((warning) => warning.severity === 'warning' &&
      (warning.code.startsWith('model_issue_') || warning.field.startsWith('suggested_tips.')) &&
      (mode === 'itemized' || !(warning.field.startsWith('items') || warning.field.startsWith('adjustments'))));
  }
  function validateReceiptImportReview(catalog, evidence, review) {
    const amounts = computeReceiptImportAmounts(catalog, review);
    acknowledge(review, receiptImportEvidenceWarnings(catalog, evidence, review.mode));
    return amounts;
  }

  globalThis.JizhangReceiptMath = Object.freeze({validateEvidence, validateReview, validateReceiptEvidence, receiptEvidenceReviewIssues,
    computeReceiptImportAmounts, validateReceiptImportReview, receiptImportEvidenceWarnings, validReceiptDate, validReceiptItemId,
    parseReceiptDecimal, receiptReviewNumericWarnings, warningToken, receiptReviewWarningToken: warningToken});
})();
