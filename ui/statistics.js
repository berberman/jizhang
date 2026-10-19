'use strict';

// The route contract is shared by filters, links, and group switching. Only
// dimensionless bounded visual ratios are converted to Number.
const Statistics = (() => {
  const keys = ['from', 'to', 'bucket', 'userId', 'currency', 'view', 'memberMeasure'];
  const date = (value) => {
    if (!/^\d{4}-\d{2}-\d{2}$/.test(value || '')) return null;
    const parsed = new Date(`${value}T00:00:00Z`);
    return Number.isFinite(parsed.getTime()) && parsed.toISOString().slice(0, 10) === value ? parsed : null;
  };
  const localDay = (d) => `${String(d.getFullYear()).padStart(4, '0')}-${String(d.getMonth() + 1).padStart(2, '0')}-${String(d.getDate()).padStart(2, '0')}`;
  const preset = (name, now = new Date()) => {
    let from = new Date(now.getFullYear(), now.getMonth(), 1), to = now;
    if (name === 'Last month') { from = new Date(now.getFullYear(), now.getMonth() - 1, 1); to = new Date(now.getFullYear(), now.getMonth(), 0); }
    if (name === 'Last 6 months') from = new Date(now.getFullYear(), now.getMonth() - 5, 1);
    if (name === 'This year') from = new Date(now.getFullYear(), 0, 1);
    return { from: localDay(from), to: localDay(to) };
  };
  const defaults = () => ({ ...preset('Last 6 months'), bucket: 'month', view: 'group', memberMeasure: 'share' });
  const validate = (query) => {
    if (!date(query.from)) return 'From: choose a valid date in YYYY-MM-DD format.';
    if (!date(query.to)) return 'To: choose a valid date in YYYY-MM-DD format.';
    const days = (date(query.to) - date(query.from)) / 86400000 + 1;
    if (days < 1 || days > 3660) return 'Choose a date range of at most 10 years';
    if (!['day', 'week', 'month'].includes(query.bucket)) return 'Bucket: choose Day, Week, or Month.';
    if (!['group', 'personal'].includes(query.view)) return 'View: choose Group or Personal.';
    if (!['share', 'paid'].includes(query.memberMeasure)) return 'Member measure: choose Share or Paid.';
    if (query.userId && !/^[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}$/i.test(query.userId)) return 'Viewing: choose a valid participant.';
    if (query.currency && !/^[A-Z]{3}$/.test(query.currency)) return 'Currency: choose a three-letter currency code.';
    return '';
  };
  const params = (query) => new URLSearchParams(keys.filter((key) => query[key] !== undefined && query[key] !== '').map((key) => [key, query[key]])).toString();
  const parse = (search) => {
    const query = defaults();
    // A half-specified range is invalid, not silently completed with today's date.
    if (search.has('from') || search.has('to')) { query.from = ''; query.to = ''; }
    for (const key of keys) if (search.has(key)) query[key] = search.get(key);
    const duplicate = keys.some((key) => search.getAll(key).length > 1);
    return { query, error: duplicate ? 'Each statistics filter must be specified only once.' : validate(query) };
  };
  const ratio = (value, total) => total > 0n ? Number((value * 1000000n) / total) / 1000000 : 0;
  const compare = (a, b) => a === b ? 0 : a > b ? -1 : 1;
  function table(headers, rows) {
    const details = node('details', null, 'statistics-data'); details.append(node('summary', 'View data'));
    const scroll = node('div', null, 'statistics-table-scroll'); scroll.tabIndex = 0; scroll.setAttribute('role', 'region'); scroll.setAttribute('aria-label', 'Exact chart data, scroll horizontally for all columns');
    const grid = node('table', null, 'statistics-table');
    const columns = headers.map((header) => typeof header === 'string' ? { label: header } : header);
    const cellClass = (column) => column.numeric ? 'statistics-numeric' : column.date ? 'statistics-date' : column.title ? 'statistics-title' : '';
    const head = node('tr'); columns.forEach((column) => { const cell = node('th', column.label, cellClass(column)); cell.scope = 'col'; head.append(cell); });
    grid.append(append(node('thead'), head)); const body = node('tbody');
    for (const row of rows) {
      const tr = node('tr');
      row.forEach((value, index) => {
        const cell = node('td', null, cellClass(columns[index]));
        if (value instanceof Node) cell.append(value);
        else cell.textContent = String(value ?? '');
        tr.append(cell);
      });
      body.append(tr);
    }
    grid.append(body); scroll.append(grid); details.append(scroll); return details;
  }
  function bars(title, currency, rows) {
    const card = append(node('section', null, 'statistics-chart stack'), node('h2', title));
    const maximum = rows.reduce((max, row) => row.amount > max ? row.amount : max, 0n);
    const selected = node('p', 'Select a bar for its exact interval and amount.', 'muted'); selected.setAttribute('role', 'status');
    const scroll = node('div', null, 'statistics-bars-scroll'); scroll.tabIndex = 0; scroll.setAttribute('aria-label', `${title}, scroll to see all intervals`);
    const plot = node('div', null, 'statistics-bars'); plot.setAttribute('role', 'group'); plot.setAttribute('aria-label', title);
    const stride = Math.max(1, Math.ceil(rows.length / 8));
    rows.forEach((row, index) => {
      const label = `${row.label}: ${money(row.amount, currency)}`;
      const bar = button('', () => { selected.textContent = label; }, 'statistics-bar', `bucket-${row.label}`);
      bar.setAttribute('aria-label', label); bar.title = label; bar.onfocus = () => { selected.textContent = label; };
      const track = node('span', null, 'statistics-bar-track'); track.setAttribute('aria-hidden', 'true');
      const fill = node('span', null, 'statistics-bar-fill'); fill.style.height = `${ratio(row.amount, maximum) * 100}%`; fill.style.minHeight = row.amount > 0n ? '2px' : '0'; track.append(fill);
      const caption = node('span', index % stride === 0 || index === rows.length - 1 ? row.shortLabel : '·', 'statistics-bar-caption'); caption.setAttribute('aria-hidden', 'true');
      bar.append(track, caption); plot.append(bar);
    });
    scroll.append(plot);
    const axis = append(node('div', null, 'statistics-axis'), node('span', money(maximum, currency)), node('span', money(maximum / 2n, currency)), node('span', money(0n, currency)));
    axis.setAttribute('aria-hidden', 'true');
    card.append(append(node('div', null, 'statistics-time-plot'), axis, scroll), selected, table([
      { label: 'Start', date: true }, { label: 'End', date: true },
      { label: `Group expenses (${currency.code})`, numeric: true }, { label: 'Expense count', numeric: true },
      { label: `Group income (${currency.code})`, numeric: true }, { label: 'Income count', numeric: true },
      { label: `Group net (${currency.code})`, numeric: true }, { label: `Selected member share (${currency.code})`, numeric: true },
      { label: `Selected member paid (${currency.code})`, numeric: true },
    ], rows.map((row) => [row.start, row.end, money(row.totalMinor, currency), row.expenseCount, money(row.incomeMinor, currency), row.incomeCount, money(row.netMinor, currency), money(row.shareMinor, currency), money(row.paidMinor, currency)])));
    return card;
  }
  function members(currency, entries, measure, onMeasure) {
    const card = append(node('section', null, 'statistics-chart stack'), node('h2', measure === 'paid' ? 'Expenses paid by member' : 'Expense shares by member'));
    const toggle = node('div', null, 'row-actions');
    for (const value of ['share', 'paid']) { const control = button(value === 'share' ? 'Share' : 'Paid', () => onMeasure(value)); control.setAttribute('aria-pressed', String(measure === value)); toggle.append(control); }
    card.append(toggle);
    const sorted = [...entries].sort((a, b) => compare(BigInt(a[`${measure}Minor`]), BigInt(b[`${measure}Minor`])) || a.user.userId.localeCompare(b.user.userId));
    const maximum = sorted.reduce((max, row) => BigInt(row[`${measure}Minor`]) > max ? BigInt(row[`${measure}Minor`]) : max, 0n);
    const list = node('div', null, 'stack'); let all = false;
    const more = button('Show all', () => { all = !all; more.textContent = all ? 'Show top ten' : 'Show all'; render(); });
    function render() {
      list.replaceChildren();
      for (const row of all ? sorted : sorted.slice(0, 10)) {
        const label = `${userLabel(row.user)} · ${money(row[`${measure}Minor`], currency)}`;
        const item = node('div', label, 'statistics-member');
        const track = node('span', null, 'statistics-member-track'); track.setAttribute('aria-hidden', 'true'); const fill = node('span'); fill.style.width = `${ratio(BigInt(row[`${measure}Minor`]), maximum) * 100}%`; track.append(fill); item.append(track); list.append(item);
      }
    }
    render(); card.append(list); if (sorted.length > 10) card.append(more);
    card.append(table([{ label: 'Member', title: true }, { label: `Allocated share (${currency.code})`, numeric: true }, { label: `Paid (${currency.code})`, numeric: true }], sorted.map((row) => [userLabel(row.user), money(row.shareMinor, currency), money(row.paidMinor, currency)]))); return card;
  }
  function expenses(subject, currency, section, openExpense) {
    const title = `${subject} · Top five expense shares`;
    const card = append(node('section', null, 'statistics-chart stack'), node('h2', title));
    const total = BigInt(section.shareMinor);
    const percent = (amount) => {
      const basisPoints = total > 0n ? amount * 10000n / total : 0n;
      return `${basisPoints / 100n}.${String(basisPoints % 100n).padStart(2, '0')}%`;
    };
    const entries = total > 0n ? section.topExpenses.map((expense, index) => ({ ...expense, rank: index + 1, amount: BigInt(expense.amountMinor) })) : [];
    if (total > 0n && BigInt(section.otherExpensesMinor) > 0n) entries.push({ title: 'Other', amount: BigInt(section.otherExpensesMinor), other: true });
    const maximum = entries.reduce((max, entry) => entry.amount > max ? entry.amount : max, 0n);
    const action = (entry, location) => {
      const control = button('Open expense', () => openExpense(entry.recordId), 'secondary statistics-expense-action', `statistics-expense-${location}-${entry.recordId}`);
      control.setAttribute('aria-label', `Open expense: ${entry.title} · ${entry.date}`); return control;
    };
    if (total === 0n) card.append(node('p', 'No allocated spending in this date range', 'empty-state'));
    else {
      card.append(node('p', `Bar scale: 0 to ${money(maximum, currency)}`, 'muted'));
      const list = node('div', null, 'statistics-expenses');
      for (const entry of entries) {
        const row = node('div', null, `statistics-expense${entry.other ? ' statistics-expense-other' : ''}`);
        const label = node('div', null, 'statistics-expense-label');
        label.append(node('strong', entry.other ? 'Other' : `${entry.rank}. ${entry.title}`));
        if (!entry.other) label.append(node('span', entry.date, 'statistics-date'));
        row.append(label, node('span', `${money(entry.amount, currency)} · ${percent(entry.amount)}`, 'statistics-expense-value'));
        const track = node('span', null, 'statistics-expense-track'); track.setAttribute('aria-hidden', 'true');
        const fill = node('span'); fill.style.width = `${ratio(entry.amount, maximum) * 100}%`; track.append(fill); row.append(track);
        if (!entry.other) row.append(action(entry, 'bar'));
        list.append(row);
      }
      card.append(list);
    }
    card.append(node('strong', `Total allocated share: ${money(total, currency)}`));
    const rows = entries.map((entry) => [entry.rank ?? '—', entry.other ? 'Other' : entry.title, entry.date ?? '—', money(entry.amount, currency), percent(entry.amount), entry.other ? '—' : action(entry, 'table')]);
    rows.push(['—', 'Total', '—', money(total, currency), percent(total), '—']);
    card.append(table([{ label: 'Rank', numeric: true }, { label: 'Expense', title: true }, { label: 'Date', date: true }, { label: `Allocated share (${currency.code})`, numeric: true }, { label: 'Percent', numeric: true }, 'Action'], rows)); return card;
  }
  return { defaults, preset, parse, params, validate, bars, members, expenses };
})();

function renderStatistics(root, group) {
  const route = state.route; const committed = { ...route.statistics.query };
  const generation = state.generation; const epoch = state.sessionGeneration; let path = route.path;
  const current = () => generation === state.generation && epoch === state.sessionGeneration && state.route.path === path && Boolean(state.adminUsername);
  const navigate = (updates) => {
    const next = groupPath(group.groupId, 'statistics', { ...committed, ...updates });
    if (location.hash === next) return loadPage();
    location.hash = next;
  };
  const filters = node('form', null, 'statistics-filters stack');
  const error = node('p', route.statistics.error, 'error'); error.setAttribute('role', 'alert'); error.id = 'statistics-filter-error';
  const dates = node('div', null, 'toolbar');
  const from = node('input'); from.type = 'date'; from.value = committed.from; from.setAttribute('aria-describedby', error.id);
  const to = node('input'); to.type = 'date'; to.value = committed.to; to.setAttribute('aria-describedby', error.id);
  const bucket = node('select'); for (const value of ['day', 'week', 'month']) { const option = node('option', capitalize(value)); option.value = value; bucket.append(option); } bucket.value = committed.bucket;
  const subject = node('select'); const owner = node('option', `Owner · ${userLabel(group.owner)}`); owner.value = ''; subject.append(owner); subject.value = '';
  if (committed.userId) { const pending = node('option', committed.userId); pending.value = committed.userId; subject.append(pending); subject.value = committed.userId; }
  const preset = node('select'); for (const name of ['Custom', 'This month', 'Last month', 'Last 6 months', 'This year']) { const option = node('option', name); preset.append(option); }
  preset.value = 'Custom';
  for (const name of ['This month', 'Last month', 'Last 6 months', 'This year']) {
    const range = Statistics.preset(name);
    if (range.from === committed.from && range.to === committed.to) preset.value = name;
  }
  preset.onchange = () => { if (preset.value !== 'Custom') navigate(Statistics.preset(preset.value)); };
  from.oninput = to.oninput = () => { preset.value = 'Custom'; };
  append(dates, labeled('Preset', preset), labeled('From', from), labeled('To (inclusive)', to), labeled('Bucket', bucket), labeled('Viewing', subject));
  const apply = node('button', 'Apply', 'primary'); apply.type = 'submit';
  filters.append(dates, error, append(node('div', null, 'row-actions'), apply, button('Reset filters', () => navigate({ ...Statistics.defaults(), userId: undefined, currency: undefined }))));
  let invalidDraft = false;
  filters.onsubmit = (event) => {
    event.preventDefault(); const draft = { ...committed, from: from.value, to: to.value, bucket: bucket.value, userId: subject.value };
    const message = Statistics.validate(draft); error.textContent = message;
    from.setAttribute('aria-invalid', String(!from.value)); to.setAttribute('aria-invalid', String(!to.value));
    invalidDraft = Boolean(message);
    if (!message) navigate(draft);
    else { expenseRequest++; content.replaceChildren(); content.removeAttribute('aria-busy'); }
  };
  const content = node('div', null, 'statistics-content stack'); content.setAttribute('aria-busy', String(!route.statistics.error));
  if (!route.statistics.error) content.append(node('p', 'Loading spending…', 'muted'));
  root.append(filters, content);
  let expenseRequest = 0;
  const expenseError = node('p', '', 'error'); expenseError.setAttribute('role', 'alert');
  async function openExpense(recordId) {
    const request = ++expenseRequest; const applies = () => current() && request === expenseRequest && state.group?.groupId === group.groupId;
    expenseError.textContent = 'Loading expense…';
    try {
      const record = await jsonFetch(`${groupApi(group.groupId)}/records/${encodeURIComponent(recordId)}`, {}, applies);
      if (!applies()) return;
      if (recordKind(record) !== 'expense') { expenseError.textContent = 'This expense is no longer available'; return; }
      expenseError.textContent = ''; openEditor('expense', record);
    } catch (failure) { if (applies() && !failure.stale) { expenseError.replaceChildren(node('span', errorMessage(failure)), button('Retry opening expense', () => openExpense(recordId))); } }
  }
  async function retry() {
    if (!current() || invalidDraft) return;
    expenseRequest++;
    content.replaceChildren(node('p', 'Loading spending…', 'muted')); content.setAttribute('aria-busy', 'true');
    const query = new URLSearchParams({ from: committed.from, to: committed.to, bucket: committed.bucket });
    if (committed.userId) query.set('userId', committed.userId);
    try {
      const data = await jsonFetch(`${groupApi(group.groupId)}/statistics?${query}`, {}, current);
      if (current()) surface.resolve(data);
    } catch (failure) { if (current() && !failure.stale) surface.reject(failure); }
  }
  const surface = {
    reject(failure) {
      content.removeAttribute('aria-busy'); content.replaceChildren(node('p', errorMessage(failure), 'error'), button('Retry', retry));
    },
    resolve(data) {
      if (!current() || invalidDraft) return;
      content.removeAttribute('aria-busy'); content.replaceChildren();
      const draftSubject = subject.value;
      subject.replaceChildren(node('option', `Owner · ${userLabel(group.owner)}`)); subject.firstChild.value = '';
      for (const user of data.participants) { const option = node('option', userLabel(user)); option.value = user.userId; subject.append(option); }
      subject.value = draftSubject;
      const currencies = [...data.currencies].sort((a, b) => a.currency.code.localeCompare(b.currency.code));
      const selected = currencies.find((section) => section.currency.code === committed.currency) || currencies.find((section) => section.currency.code === group.defaultCurrency.code) || currencies[0];
      // Persist a concrete currency choice without adding a history entry. This
      // is presentation state only; the in-flight endpoint query is unchanged.
      if (selected && selected.currency.code !== committed.currency) {
        committed.currency = selected.currency.code;
        state.route.statistics.query.currency = committed.currency;
        state.route.path = groupPath(group.groupId, 'statistics', committed);
        history.replaceState(null, '', state.route.path);
      } else if (!selected && committed.currency) {
        delete committed.currency; delete state.route.statistics.query.currency;
        state.route.path = groupPath(group.groupId, 'statistics', committed); history.replaceState(null, '', state.route.path);
      }
      path = state.route.path;
      if (group.currencyMode === 'multi' && selected) {
        const currency = node('select'); for (const section of currencies) { const option = node('option', section.currency.code); option.value = section.currency.code; currency.append(option); } currency.value = selected.currency.code;
        currency.onchange = () => navigate({ currency: currency.value }); content.append(labeled('Currency', currency));
      }
      const views = node('div', null, 'row-actions'); views.setAttribute('aria-label', 'Spending view');
      for (const view of ['group', 'personal']) { const control = button(capitalize(view), () => navigate({ view })); control.setAttribute('aria-pressed', String(committed.view === view)); views.append(control); } content.append(views);
      if (!selected) { content.append(node('p', 'No activity in this date range', 'empty-state'), button('Change dates', () => from.focus())); return; }
      const currency = selected.currency; const personal = userLabel(data.user);
      const summary = node('div', null, 'statistics-summary');
      for (const [label, value] of [['Group spending', money(selected.totalMinor, currency)], ['Expense count', selected.expenseCount], ['Group income', money(selected.incomeMinor, currency)], ['Income count', selected.incomeCount], ['Net (income − expenses)', money(selected.netMinor, currency)], [`${personal} · Expense share`, money(selected.shareMinor, currency)], [`${personal} · Expense paid`, money(selected.paidMinor, currency)]]) summary.append(append(node('div', null, 'statistics-summary-item'), node('span', label, 'muted'), node('strong', value)));
      content.append(summary);
      const charts = node('div', null, 'statistics-charts');
      const personalView = committed.view === 'personal';
      charts.append(Statistics.bars(personalView ? `${personal} · Expense share over time` : 'Group expenses over time', currency, selected.buckets.map((entry) => ({ ...entry, label: `${entry.start} – ${entry.end}`, shortLabel: entry.start, amount: BigInt(personalView ? entry.shareMinor : entry.totalMinor) }))));
      charts.append(personalView ? Statistics.expenses(personal, currency, selected, openExpense) : Statistics.members(currency, selected.members, committed.memberMeasure, (memberMeasure) => navigate({ memberMeasure })));
      content.append(charts, expenseError);
    },
  };
  return surface;
}
