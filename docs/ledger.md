# Ledger behavior

The Flutter client and the same-origin admin console use the same ledger API.
Unless a section names an interface, the contracts below apply to both frontends.
For authentication and session failures, see
[session and failure behavior](authentication.md#session-and-failure-behavior).

## Exact money and currencies

Amounts use integer minor units, never floating point. The API represents money
as decimal strings; clients use exact integer arithmetic and the currency
catalog's precision. Equal, weighted-share, percentage, and exact splits use
deterministic rounding. Source and settlement allocations round independently
in deterministic UUID order.

Individual record amounts and split values are bounded by the supported signed
64-bit storage range before decimal-string conversion. Manual rates allow at most
18 significant integer and 18 fractional digits after zero normalization. CSV
amounts apply the catalog precision and the same storage bound before conversion.
Report and balance totals remain signed arbitrary-precision integers.

Saved settlement allocations and exchange-rate provenance remain part of the
ledger. Displaying a report does not fetch a new rate or recalculate saved
allocations. Single-base groups settle in their configured currency;
multi-currency groups keep balances and suggested payments separate per currency.

Expenses debit participants' shares and credit the payer. Income debits its
receiver and credits beneficiaries. Payments move balances between members
without counting as spending or income.

## Navigation, identities, and editors

Groups is the authenticated home in the Flutter client. Its app-bar profile
control opens Account; Back to groups returns home. Group sections share a
persistent roster/header, use transitionless routes, and scroll only their own
content. Receipt expansion state is independent of the page's saved scroll
offset.

User/group choices are searchable and UUID-backed. Member discovery is available
only to the group owner. Ordinary group and ledger views show public identities,
not email. Group rows include short IDs to distinguish identical names. Split
filtering never removes selected participants or weights. Former members remain
visible and must be re-added or explicitly replaced where allowed before an
edited record can be saved.

In Flutter, controls have a 48px minimum size, and forms pair related fields on
wider layouts and stack them on phones. Dirty editors ask for an explicit discard
decision before navigation. Failed saves retain input, and pending actions cannot
be submitted twice. A lost mutation response is uncertain: inspect refreshed data
before resubmitting. Mutations are not automatically replayed. Read failures
expose an explicit Retry action.

## Record notes

Every expense, income, and payment supports an optional plain-text multiline
note, independent of receipt notes. The API always returns `note` as text or
null. On a full record PUT, an omitted, null, or whitespace-only note clears the
value. Nonblank Unicode and newlines are preserved exactly. NUL is rejected with
HTTP 400. HTML and Markdown are not interpreted.

## Record queries and sorting

Records support an editable query plus structured AND filters. Search or Enter
applies the draft. Sorting uses the last applied query; Clear resets both the
draft and applied query. Errors retain editable input and label retained results
as previous results.

```text
type:expense payer:@alice date:2026-10-01..2026-10-31 currency:USD amount:>=25 title:"team lunch" note:"invoice 42"
```

Qualifiers are `title`, `note`, `type`, `payer`, `payee`, `member`, `date`,
`created` (UTC calendar day), `currency`, and `amount` (original major units).
Dates and amounts support `=`, `<`, `<=`, `>`, `>=`, or inclusive `lower..upper`.
Amount comparisons require exactly one currency, respect its precision, and
never convert currencies.

Quoted identities match public display names; unquoted handles and UUIDs match
exactly. Identity pickers include labeled former members and submit UUIDs, never
email. Bare text searches public record text. Quote phrases and escape only
quotes/backslashes inside quotes. Terms, including repeated filters, combine
with AND. Both frontends filter the complete authorized ledger before paging.

Default sorting is newest transaction date; created-time sorting is separate.
UUID ties stay ascending in either direction. Amount sorting groups currencies.

## Receipts and CSV imports

Receipts bundle expenses; they are not image uploads. A receipt summary shows its
own loaded expense total, never a paginated global total. Receipt bundles and
the five-column CSV import are expense-only.

CSV imports are atomic. The editable source is exactly what is sent, whether
selected from a UTF-8 file or pasted. Record notes do not change the five-column
format; imported records have null record notes. For example:

```csv
title,amount,paidBy,split,date
Dinner,90,alice,"alice,bob,bob",2026-10-14
```

## Income

Use **New income** in either interface. Income has a positive amount, a receiver,
and beneficiaries' entitlements. The receiver need not be a beneficiary. Its
shared split editor supports equal, shares, percentages, and exact amounts,
with the same FX controls and saved-rate provenance as expenses.

For example, Bob receiving 60.00 equally owned by Alice and Bob makes Bob owe
Alice 30.00. Income is not a negative expense or a member payment.

User API POST `/groups/:groupId/records/income` and PUT
`/groups/:groupId/records/income/:recordId` take the expense request shape with
`receivedByUserId` instead of `byUserId`. Responses have `tag: "IncomeRecord"`
and `receivedBy` instead of `paidBy`. Admin routes use the `/admin` prefix.
Generic record listing, detail, and deletion include income.

## All-time balances

Balances use saved settlement allocations across all time, not the date-filtered
statistics report. Each member has six exact role totals: expenses paid and
share, income received and entitlement, and payments sent and received. The net
reconciles as:

```text
paid − share − income received + entitlement + sent − received
```

Every distinct record appears once in a member's breakdown, including zero-net
self allocations. Expand a member for the date, kind, full saved settlement
amount, and signed member balance impact. The full record amount is distinct
from that member's impact.

Member sorting is local and supports magnitude, signed net, public name, and
activity count. Currency selection, sort, and per-currency expansion survive
local changes. Suggested payments remain server-authoritative and open a
prefilled review form; they save only on submission. Payments involving former
members remain disabled with guidance to re-add them.

## Statistics

Statistics are date-filtered and show group spending, group income, and net
(`income − expenses`), with separate expense and income counts. API `totalMinor`
retains its expense meaning; currency totals and buckets also require
`incomeMinor`, `incomeCount`, and `netMinor`. Net can be negative.

Personal shares, amounts paid, member/time charts, and top expenses remain
expense-only. Paid means money advanced for expenses, not consumption. Income
totals stay groupwide in Personal view. Payments affect all-time balances but
are excluded from statistics totals.

Personal Stats rank the selected member's five largest allocated expense
shares, not spending categories. Bars preserve backend order and show date,
exact share, and a truncated two-decimal percent. Positive Other is the exact
sum outside the top five, not a record. Other and total have no record action.
Expense actions open the actual group-scoped record. Zero-share periods show no
expense bars while group income and net remain visible.

### Accessible chart data

Every **View data** disclosure contains a real table with its own horizontal
scrolling. Currency amounts remain exact, selectable text, including values
above JavaScript's safe integer range. Scoped expense actions remain keyboard
reachable.

- Time tables include every zero-filled interval, group expense/income amounts
  and counts, signed group net, and the selected member's share and paid totals.
- Member tables include all members regardless of the chart's top-ten toggle.
- Personal tables include ranked expenses, dates, shares, percentages, actions,
  Other, and total.
