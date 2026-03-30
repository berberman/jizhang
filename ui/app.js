const state = {
  adminUsername: '',
  activeTab: 'overview',
  users: { query: '', offset: 0, limit: 20, sort: 'username' },
  admins: { query: '', offset: 0, limit: 20, sort: 'username' },
  groups: { query: '', offset: 0, limit: 20, sort: 'groupName' },
  records: { query: '', offset: 0, limit: 20, sort: 'title' },
  receipts: { query: '', offset: 0, limit: 20, sort: 'note' },
  selectedGroupId: null,
  editingExpenseId: null,
  editingTransferId: null,
  editingReceiptId: null,
};

const $ = (selector) => document.querySelector(selector);

function setHidden(selector, hidden) {
  $(selector).classList.toggle('hidden', hidden);
}

function showGlobalError(message) {
  const node = $('#global-error');
  node.textContent = message;
  setHidden('#global-error', false);
}

function clearGlobalError() {
  $('#global-error').textContent = '';
  setHidden('#global-error', true);
}

function readCookie(name) {
  const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const match = document.cookie.match(new RegExp(`(?:^|; )${escaped}=([^;]*)`));
  return match ? decodeURIComponent(match[1]) : '';
}

function authHeaders() {
  const xsrf = readCookie('XSRF-TOKEN');
  return xsrf ? { 'X-XSRF-TOKEN': xsrf } : {};
}

async function jsonFetch(url, options = {}) {
  const response = await fetch(url, {
    ...options,
    credentials: 'same-origin',
    headers: {
      'Content-Type': 'application/json',
      ...authHeaders(),
      ...(options.headers || {}),
    },
  });
  if (!response.ok) {
    const text = await response.text();
    if (response.status === 401) {
      clearSession();
      renderAppShell();
    }
    throw new Error(text || `${response.status} ${response.statusText}`);
  }
  if (response.status === 204) return null;
  const text = await response.text();
  return text ? JSON.parse(text) : null;
}

function clearSession() {
  state.adminUsername = '';
}

function buildPageUrl(base, params) {
  const url = new URL(base, window.location.origin);
  if (params.query) url.searchParams.set('query', params.query);
  url.searchParams.set('offset', String(params.offset));
  url.searchParams.set('limit', String(params.limit));
  url.searchParams.set('sort', params.sort);
  return url.toString();
}

function renderPager(containerId, page, onPrev, onNext) {
  const container = $(containerId);
  const info = page.pageInfo;
  container.innerHTML = '';
  const label = document.createElement('span');
  label.textContent = `${info.total === 0 ? 0 : info.offset + 1}-${Math.min(info.offset + info.limit, info.total)} / ${info.total}`;
  const prev = document.createElement('button');
  prev.className = 'ghost';
  prev.textContent = 'Prev';
  prev.disabled = !info.hasPrev;
  prev.onclick = onPrev;
  const next = document.createElement('button');
  next.className = 'ghost';
  next.textContent = 'Next';
  next.disabled = !info.hasNext;
  next.onclick = onNext;
  container.append(prev, label, next);
}

function clientPage(items, params, accessor) {
  const sortedItems = [...items].sort((left, right) => {
    const leftValue = String(accessor(left)).toLowerCase();
    const rightValue = String(accessor(right)).toLowerCase();
    return leftValue.localeCompare(rightValue);
  });
  const ordered = params.sort.startsWith('-') ? sortedItems.reverse() : sortedItems;
  const total = ordered.length;
  const pageItems = ordered.slice(params.offset, params.offset + params.limit);
  return {
    items: pageItems,
    pageInfo: {
      offset: params.offset,
      limit: params.limit,
      total,
      hasNext: params.offset + params.limit < total,
      hasPrev: params.offset > 0,
    },
  };
}

function guarded(action) {
  return async (...args) => {
    clearGlobalError();
    try {
      await action(...args);
    } catch (error) {
      showGlobalError(error.message);
    }
  };
}

function usernameValue(value) {
  return typeof value === 'string' ? value : value?.unUsername || value?.username || '';
}

function normalizeSplit(split) {
  return {
    username: usernameValue(split.username),
    share: Number(split.share),
  };
}

function normalizeExpenseRecord(record) {
  return {
    title: record.title,
    amount: Number(record.amount),
    byUsername: usernameValue(record.byUsername || record.paidBy?.username),
    date: record.date,
    splits: (record.splits || []).map((split) => normalizeSplit(split.user ? { username: split.user.username, share: split.share } : split)),
  };
}

function createSplitRow(username = '', share = 1) {
  const row = document.createElement('div');
  row.className = 'split-row';
  row.innerHTML = `
    <input type="text" class="split-username" placeholder="username" value="${username}" required />
    <input type="number" class="split-share" min="1" step="1" value="${share}" required />
    <button type="button" class="ghost">Remove</button>
  `;
  row.querySelector('button').onclick = () => row.remove();
  return row;
}

function collectSplits(containerSelector) {
  return Array.from($(containerSelector).querySelectorAll('.split-row')).map((row) => ({
    username: row.querySelector('.split-username').value.trim(),
    share: Number(row.querySelector('.split-share').value),
  }));
}

function createReceiptRecordCard(record = null) {
  const card = document.createElement('div');
  card.className = 'record-editor';
  card.innerHTML = `
    <div class="record-grid">
      <input type="text" class="receipt-record-title" placeholder="Title" required />
      <input type="number" class="receipt-record-amount" min="0" step="0.01" placeholder="Amount" required />
      <input type="text" class="receipt-record-payer" placeholder="Paid by username" required />
      <input type="date" class="receipt-record-date" required />
    </div>
    <div class="stack gap-sm">
      <div class="section-header compact">
        <h5>Splits</h5>
        <button type="button" class="ghost add-record-split-btn">Add split</button>
      </div>
      <div class="stack gap-sm receipt-record-splits"></div>
    </div>
    <button type="button" class="ghost remove-record-btn">Remove record</button>
  `;
  card.querySelector('.add-record-split-btn').onclick = () => {
    card.querySelector('.receipt-record-splits').appendChild(createSplitRow());
  };
  card.querySelector('.remove-record-btn').onclick = () => card.remove();
  const splitsContainer = card.querySelector('.receipt-record-splits');
  if (record) {
    card.querySelector('.receipt-record-title').value = record.title;
    card.querySelector('.receipt-record-amount').value = record.amount;
    card.querySelector('.receipt-record-payer').value = usernameValue(record.byUsername || record.paidBy?.username);
    card.querySelector('.receipt-record-date').value = record.date;
    (record.splits || []).forEach((split) => {
      const splitValue = split.user ? { username: split.user.username, share: split.share } : split;
      splitsContainer.appendChild(createSplitRow(usernameValue(splitValue.username), splitValue.share));
    });
  }
  if (!splitsContainer.children.length) splitsContainer.appendChild(createSplitRow());
  return card;
}

function collectReceiptRecords() {
  return Array.from($('#receipt-records-list').querySelectorAll('.record-editor')).map((card) => ({
    title: card.querySelector('.receipt-record-title').value.trim(),
    amount: Number(card.querySelector('.receipt-record-amount').value),
    byUsername: card.querySelector('.receipt-record-payer').value.trim(),
    date: card.querySelector('.receipt-record-date').value,
    splits: Array.from(card.querySelectorAll('.split-row')).map((row) => ({
      username: row.querySelector('.split-username').value.trim(),
      share: Number(row.querySelector('.split-share').value),
    })),
  }));
}

function resetExpenseForm() {
  state.editingExpenseId = null;
  $('#expense-record-form').reset();
  $('#expense-record-id').value = '';
  $('#expense-form-title').textContent = 'Create expense';
  $('#expense-splits-list').innerHTML = '';
  $('#expense-splits-list').appendChild(createSplitRow());
  setHidden('#expense-cancel-btn', true);
}

function resetTransferForm() {
  state.editingTransferId = null;
  $('#transfer-record-form').reset();
  $('#transfer-record-id').value = '';
  $('#transfer-form-title').textContent = 'Create transfer';
  setHidden('#transfer-cancel-btn', true);
}

function resetReceiptForm() {
  state.editingReceiptId = null;
  $('#receipt-form').reset();
  $('#receipt-id-input').value = '';
  $('#receipt-form-title').textContent = 'Create receipt';
  $('#receipt-uploaded-by-input').disabled = false;
  $('#receipt-records-list').innerHTML = '';
  $('#receipt-records-list').appendChild(createReceiptRecordCard());
  setHidden('#receipt-cancel-btn', true);
}

function startExpenseEdit(record) {
  state.editingExpenseId = record.recordId;
  $('#expense-record-id').value = record.recordId;
  $('#expense-form-title').textContent = 'Edit expense';
  $('#expense-title-input').value = record.title;
  $('#expense-amount-input').value = record.amount;
  $('#expense-payer-input').value = record.paidBy.username;
  $('#expense-date-input').value = record.date;
  $('#expense-splits-list').innerHTML = '';
  record.splits.forEach((split) => {
    $('#expense-splits-list').appendChild(createSplitRow(split.user.username, split.share));
  });
  setHidden('#expense-cancel-btn', false);
  resetTransferForm();
}

function startTransferEdit(record) {
  state.editingTransferId = record.recordId;
  $('#transfer-record-id').value = record.recordId;
  $('#transfer-form-title').textContent = 'Edit transfer';
  $('#transfer-amount-input').value = record.amount;
  $('#transfer-payer-input').value = record.paidBy.username;
  $('#transfer-receiver-input').value = record.transferTo.username;
  $('#transfer-date-input').value = record.date;
  setHidden('#transfer-cancel-btn', false);
  resetExpenseForm();
}

function startReceiptEdit(receipt) {
  state.editingReceiptId = receipt.receiptId;
  $('#receipt-id-input').value = receipt.receiptId;
  $('#receipt-form-title').textContent = 'Edit receipt';
  $('#receipt-note-input').value = receipt.note;
  $('#receipt-uploaded-by-input').value = receipt.uploadedBy.username;
  $('#receipt-uploaded-by-input').disabled = true;
  $('#receipt-records-list').innerHTML = '';
  receipt.records.map(normalizeExpenseRecord).forEach((record) => {
    $('#receipt-records-list').appendChild(createReceiptRecordCard(record));
  });
  setHidden('#receipt-cancel-btn', false);
}

function switchTab(tab) {
  state.activeTab = tab;
  document.querySelectorAll('.nav-btn').forEach((button) => {
    button.classList.toggle('active', button.dataset.tab === tab);
  });
  document.querySelectorAll('.tab-panel').forEach((panel) => panel.classList.add('hidden'));
  $(`#tab-${tab}`).classList.remove('hidden');
  $('#page-title').textContent = tab[0].toUpperCase() + tab.slice(1);
}

function renderAppShell() {
  const loggedIn = Boolean(state.adminUsername);
  setHidden('#login-view', loggedIn);
  setHidden('#dashboard-view', !loggedIn);
  $('#admin-identity').textContent = loggedIn ? `Signed in as ${state.adminUsername}` : '';
}

function setGroupDetailVisible(visible) {
  setHidden('#group-detail', !visible);
  $('#groups-workspace').classList.toggle('detail-hidden', !visible);
}

async function handleLogin(event) {
  event.preventDefault();
  const form = new FormData(event.currentTarget);
  $('#login-error').textContent = '';
  setHidden('#login-error', true);
  try {
    const response = await jsonFetch('/admin/auth/session/login', {
      method: 'POST',
      body: JSON.stringify({
        username: String(form.get('username') || ''),
        password: String(form.get('password') || ''),
      }),
      headers: {},
    });
    state.adminUsername = response.summaryUsername;
    renderAppShell();
    await loadOverview();
  } catch (error) {
    $('#login-error').textContent = error.message;
    setHidden('#login-error', false);
  }
}

async function logout() {
  await jsonFetch('/admin/auth/session/logout', { method: 'POST' }).catch(() => null);
  clearSession();
  renderAppShell();
}

async function loadOverview() {
  clearGlobalError();
  switchTab('overview');
  const [users, admins, groups] = await Promise.all([
    jsonFetch(buildPageUrl('/admin/users', state.users)),
    jsonFetch(buildPageUrl('/admin/admins', state.admins)),
    jsonFetch(buildPageUrl('/admin/groups', state.groups)),
  ]);
  $('#stat-users').textContent = users.pageInfo.total;
  $('#stat-admins').textContent = admins.pageInfo.total;
  $('#stat-groups').textContent = groups.pageInfo.total;
}

async function loadUsers() {
  clearGlobalError();
  switchTab('users');
  const page = await jsonFetch(buildPageUrl('/admin/users', state.users));
  const tbody = $('#users-table');
  tbody.innerHTML = '';
  page.items.forEach((user) => {
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>${user.username}</td><td class="mono">${user.userId}</td><td><button class="danger">Delete</button></td>`;
    tr.querySelector('button').onclick = guarded(async () => {
      if (!confirm(`Delete user ${user.username}?`)) return;
      await jsonFetch(`/admin/users/${user.userId}`, { method: 'DELETE' });
      await loadUsers();
      await loadOverview();
    });
    tbody.appendChild(tr);
  });
  renderPager('#users-pager', page, () => { state.users.offset -= state.users.limit; loadUsers(); }, () => { state.users.offset += state.users.limit; loadUsers(); });
}

async function loadAdmins() {
  clearGlobalError();
  switchTab('admins');
  const page = await jsonFetch(buildPageUrl('/admin/admins', state.admins));
  const tbody = $('#admins-table');
  tbody.innerHTML = '';
  page.items.forEach((admin) => {
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>${admin.summaryUsername}</td><td class="mono">${admin.adminId}</td>`;
    tbody.appendChild(tr);
  });
  renderPager('#admins-pager', page, () => { state.admins.offset -= state.admins.limit; loadAdmins(); }, () => { state.admins.offset += state.admins.limit; loadAdmins(); });
}

async function loadGroups() {
  clearGlobalError();
  switchTab('groups');
  if (!state.selectedGroupId) {
    setGroupDetailVisible(false);
  }
  const page = await jsonFetch(buildPageUrl('/admin/groups', state.groups));
  const tbody = $('#groups-table');
  tbody.innerHTML = '';
  page.items.forEach((group) => {
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>${group.groupName}</td><td>${group.owner.username}</td><td>${group.members.length}</td>`;
    tr.onclick = guarded(async () => loadGroupDetail(group.groupId));
    tbody.appendChild(tr);
  });
  renderPager('#groups-pager', page, () => { state.groups.offset -= state.groups.limit; loadGroups(); }, () => { state.groups.offset += state.groups.limit; loadGroups(); });
}

function renderMembers(group) {
  const list = $('#group-members-list');
  list.innerHTML = '';
  group.members.forEach((member) => {
    const li = document.createElement('li');
    li.innerHTML = `<span>${member.username}</span><button class="ghost">Remove</button>`;
    li.querySelector('button').onclick = guarded(async () => {
      await jsonFetch(`/admin/groups/${group.groupId}/members/${member.username}`, { method: 'DELETE' });
      await loadGroupDetail(group.groupId);
      await loadGroups();
    });
    list.appendChild(li);
  });
}

function renderReport(report) {
  const root = $('#group-report');
  const balances = report.balances.map((b) => `<li>${b.user.username}: ${b.totalAmount.toFixed(2)}</li>`).join('');
  const settlements = report.settlements.map((s) => `<li>${s.fromUser.username} → ${s.toUser.username}: ${s.amount.toFixed(2)}</li>`).join('');
  root.innerHTML = `
    <div class="grid-2">
      <div><h4>Balances</h4><ul>${balances || '<li>No balances</li>'}</ul></div>
      <div><h4>Settlements</h4><ul>${settlements || '<li>No settlements</li>'}</ul></div>
    </div>
  `;
}

function renderRecords(groupId, page) {
  const tbody = $('#records-table');
  tbody.innerHTML = '';
  page.items.forEach((record) => {
    const type = record.transferTo ? 'Transfer' : 'Expense';
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>${record.title}</td><td>${record.amount.toFixed(2)}</td><td>${type}</td><td>${record.date}</td><td><button class="ghost">Edit</button></td><td><button class="danger">Delete</button></td>`;
    tr.querySelector('.ghost').onclick = () => {
      if (record.transferTo) startTransferEdit(record);
      else startExpenseEdit(record);
    };
    tr.querySelector('.danger').onclick = guarded(async () => {
      await jsonFetch(`/admin/groups/${groupId}/records/${record.recordId}`, { method: 'DELETE' });
      await loadGroupDetail(groupId);
    });
    tbody.appendChild(tr);
  });
  renderPager('#records-pager', page, () => { state.records.offset -= state.records.limit; loadGroupDetail(groupId); }, () => { state.records.offset += state.records.limit; loadGroupDetail(groupId); });
}

function renderReceipts(groupId, page) {
  const tbody = $('#receipts-table');
  tbody.innerHTML = '';
  page.items.forEach((receipt) => {
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>${receipt.note}</td><td>${receipt.uploadedBy.username}</td><td>${receipt.records.length}</td><td><button class="ghost">Edit</button></td><td><button class="danger">Delete</button></td>`;
    tr.querySelector('.ghost').onclick = () => startReceiptEdit(receipt);
    tr.querySelector('.danger').onclick = guarded(async () => {
      await jsonFetch(`/admin/groups/${groupId}/receipts/${receipt.receiptId}`, { method: 'DELETE' });
      await loadGroupDetail(groupId);
    });
    tbody.appendChild(tr);
  });
  renderPager('#receipts-pager', page, () => { state.receipts.offset -= state.receipts.limit; loadGroupDetail(groupId); }, () => { state.receipts.offset += state.receipts.limit; loadGroupDetail(groupId); });
}

async function loadGroupDetail(groupId) {
  clearGlobalError();
  state.selectedGroupId = groupId;
  setGroupDetailVisible(true);
  const [group, report, records, receipts] = await Promise.all([
    jsonFetch(`/admin/groups/${groupId}`),
    jsonFetch(`/admin/groups/${groupId}/report`),
    jsonFetch(`/admin/groups/${groupId}/records`),
    jsonFetch(`/admin/groups/${groupId}/receipts`),
  ]);
  $('#group-detail-name').textContent = group.groupName;
  $('#group-detail-meta').textContent = `Owner: ${group.owner.username} · Members: ${group.members.length}`;
  $('#rename-group-input').value = group.groupName;
  resetExpenseForm();
  resetTransferForm();
  resetReceiptForm();
  renderMembers(group);
  renderReport(report);
  renderRecords(groupId, clientPage(records, state.records, (record) => record.title));
  renderReceipts(groupId, clientPage(receipts, state.receipts, (receipt) => receipt.note));
}

function bindForms() {
  $('#login-form').addEventListener('submit', handleLogin);
  $('#logout-btn').addEventListener('click', logout);
  $('#expense-add-split-btn').addEventListener('click', () => $('#expense-splits-list').appendChild(createSplitRow()));
  $('#receipt-add-record-btn').addEventListener('click', () => $('#receipt-records-list').appendChild(createReceiptRecordCard()));
  $('#expense-cancel-btn').addEventListener('click', resetExpenseForm);
  $('#transfer-cancel-btn').addEventListener('click', resetTransferForm);
  $('#receipt-cancel-btn').addEventListener('click', resetReceiptForm);

  $('#create-user-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    const formEl = event.currentTarget;
    const form = new FormData(formEl);
    await jsonFetch('/admin/users', { method: 'POST', body: JSON.stringify({ createUsername: form.get('username'), createPassword: form.get('password') }) });
    formEl.reset();
    await loadUsers();
  }));

  $('#create-admin-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    const formEl = event.currentTarget;
    const form = new FormData(formEl);
    await jsonFetch('/admin/admins', { method: 'POST', body: JSON.stringify({ createAdminUsername: form.get('username'), createAdminPassword: form.get('password') }) });
    formEl.reset();
    await loadAdmins();
  }));

  $('#users-filter-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    state.users.query = $('#users-query').value.trim();
    state.users.sort = $('#users-sort').value;
    state.users.offset = 0;
    await loadUsers();
  }));

  $('#admins-filter-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    state.admins.query = $('#admins-query').value.trim();
    state.admins.sort = $('#admins-sort').value;
    state.admins.offset = 0;
    await loadAdmins();
  }));

  $('#groups-filter-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    state.groups.query = $('#groups-query').value.trim();
    state.groups.sort = $('#groups-sort').value;
    state.groups.offset = 0;
    await loadGroups();
  }));

  $('#create-group-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    const formEl = event.currentTarget;
    await jsonFetch('/admin/groups', {
      method: 'POST',
      body: JSON.stringify({
        createGroupName: $('#create-group-name-input').value.trim(),
        ownerUsername: $('#create-group-owner-input').value.trim(),
      }),
    });
    formEl.reset();
    await loadGroups();
  }));

  $('#rename-group-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    if (!state.selectedGroupId) return;
    await jsonFetch(`/admin/groups/${state.selectedGroupId}`, { method: 'PUT', body: JSON.stringify($('#rename-group-input').value.trim()) });
    await loadGroupDetail(state.selectedGroupId);
    await loadGroups();
  }));

  $('#add-member-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    if (!state.selectedGroupId) return;
    const formEl = event.currentTarget;
    await jsonFetch(`/admin/groups/${state.selectedGroupId}/members`, { method: 'POST', body: JSON.stringify($('#member-username-input').value.trim()) });
    formEl.reset();
    await loadGroupDetail(state.selectedGroupId);
  }));

  $('#transfer-owner-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    if (!state.selectedGroupId) return;
    const formEl = event.currentTarget;
    await jsonFetch(`/admin/groups/${state.selectedGroupId}/owner`, { method: 'PUT', body: JSON.stringify($('#new-owner-input').value.trim()) });
    formEl.reset();
    await loadGroupDetail(state.selectedGroupId);
    await loadGroups();
  }));

  $('#expense-record-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    if (!state.selectedGroupId) return;
    const payload = {
      title: $('#expense-title-input').value.trim(),
      amount: Number($('#expense-amount-input').value),
      byUsername: $('#expense-payer-input').value.trim(),
      date: $('#expense-date-input').value,
      splits: collectSplits('#expense-splits-list'),
    };
    if (state.editingExpenseId) {
      await jsonFetch(`/admin/groups/${state.selectedGroupId}/records/expense/${state.editingExpenseId}`, { method: 'PUT', body: JSON.stringify(payload) });
    } else {
      await jsonFetch(`/admin/groups/${state.selectedGroupId}/records/expense`, { method: 'POST', body: JSON.stringify(payload) });
    }
    resetExpenseForm();
    await loadGroupDetail(state.selectedGroupId);
  }));

  $('#transfer-record-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    if (!state.selectedGroupId) return;
    const payload = {
      amount: Number($('#transfer-amount-input').value),
      byUsername: $('#transfer-payer-input').value.trim(),
      toUsername: $('#transfer-receiver-input').value.trim(),
      date: $('#transfer-date-input').value,
    };
    if (state.editingTransferId) {
      await jsonFetch(`/admin/groups/${state.selectedGroupId}/records/transfer/${state.editingTransferId}`, { method: 'PUT', body: JSON.stringify(payload) });
    } else {
      await jsonFetch(`/admin/groups/${state.selectedGroupId}/records/transfer`, { method: 'POST', body: JSON.stringify(payload) });
    }
    resetTransferForm();
    await loadGroupDetail(state.selectedGroupId);
  }));

  $('#receipt-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    if (!state.selectedGroupId) return;
    const payload = {
      note: $('#receipt-note-input').value.trim(),
      records: collectReceiptRecords(),
    };
    if (state.editingReceiptId) {
      await jsonFetch(`/admin/groups/${state.selectedGroupId}/receipts/${state.editingReceiptId}`, { method: 'PUT', body: JSON.stringify(payload) });
    } else {
      await jsonFetch(`/admin/groups/${state.selectedGroupId}/receipts`, {
        method: 'POST',
        body: JSON.stringify({
          uploadedByUsername: $('#receipt-uploaded-by-input').value.trim(),
          note: payload.note,
          records: payload.records,
        }),
      });
    }
    resetReceiptForm();
    await loadGroupDetail(state.selectedGroupId);
  }));

  $('#import-csv-form').addEventListener('submit', guarded(async (event) => {
    event.preventDefault();
    if (!state.selectedGroupId) return;
    const formEl = event.currentTarget;
    const file = $('#import-csv-file').files?.[0];
    const csvText = file ? await file.text() : $('#import-csv-text').value;
    const response = await fetch(`/admin/groups/${state.selectedGroupId}/import`, {
      method: 'POST',
      credentials: 'same-origin',
      headers: {
        ...authHeaders(),
        'Content-Type': 'text/csv',
      },
      body: csvText,
    });
    if (!response.ok) {
      const text = await response.text();
      throw new Error(text || `${response.status} ${response.statusText}`);
    }
    formEl.reset();
    $('#import-csv-text').value = '';
    await loadGroupDetail(state.selectedGroupId);
  }));

  $('#delete-group-btn').addEventListener('click', guarded(async () => {
    if (!state.selectedGroupId || !confirm('Delete this group?')) return;
    await jsonFetch(`/admin/groups/${state.selectedGroupId}`, { method: 'DELETE' });
    state.selectedGroupId = null;
    setGroupDetailVisible(false);
    await loadGroups();
  }));

  document.querySelectorAll('.nav-btn').forEach((button) => {
    button.addEventListener('click', guarded(async () => {
      if (button.dataset.tab === 'overview') await loadOverview();
      if (button.dataset.tab === 'users') await loadUsers();
      if (button.dataset.tab === 'admins') await loadAdmins();
      if (button.dataset.tab === 'groups') await loadGroups();
    }));
  });
}

async function bootstrap() {
  bindForms();
  resetExpenseForm();
  resetTransferForm();
  resetReceiptForm();
  renderAppShell();
  try {
    const session = await jsonFetch('/admin/auth/session/me', { method: 'GET' });
    state.adminUsername = session.summaryUsername;
    renderAppShell();
    await loadOverview();
  } catch (_error) {
    clearSession();
    renderAppShell();
  }
}

window.addEventListener('DOMContentLoaded', bootstrap);
