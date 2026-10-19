'use strict';

const $ = (selector) => document.querySelector(selector);
const newLists = () => Object.fromEntries(['users', 'admins', 'groups', 'records', 'receipts'].map((kind) => [kind, {
  query: '', offset: 0, limit: 20, sort: ({ groups: 'groupName', records: '-date', receipts: 'note' })[kind] || 'username',
}]));
const state = {
  adminUsername: '', route: { path: '#/overview', page: 'overview', groupId: null },
  lists: newLists(), group: null, editor: null, confirmation: null,
  recordView: null, reportView: null,
  generation: 0, sessionGeneration: 0, navigationGeneration: 0, loginPending: false,
};

function node(tag, text, className) {
  const element = document.createElement(tag);
  if (text !== undefined && text !== null) element.textContent = String(text);
  if (className) element.className = className;
  return element;
}
function append(parent, ...children) { parent.append(...children.filter(Boolean)); return parent; }
function button(label, action, className = 'secondary', key = label) {
  const element = node('button', label, className);
  element.type = 'button'; element.dataset.focusKey = key;
  element.onclick = guarded(action);
  if (state.editor?.pending) { element.disabled = true; element.dataset.wasDisabled = 'false'; }
  return element;
}
function link(label, path, className = '') {
  const element = node('a', label, className); element.href = path; element.dataset.focusKey = path;
  return element;
}
function errorMessage(error) { return error?.message || 'An unexpected error occurred'; }
function showError(target, error) {
  if (error.stale) return;
  const element = typeof target === 'function' ? target() : target;
  if (element?.isConnected) element.textContent = errorMessage(error);
}
function guarded(action, target = () => state.confirmation ? $('#confirm-error') : state.editor ? $('#editor-error') : state.adminUsername ? $('#page-error') : $('#login-error')) {
  return async (...args) => {
    try { return await action(...args); }
    catch (error) { showError(target, error); }
  };
}
function staleError() { return Object.assign(new Error('Request no longer applies'), { stale: true }); }
function readCookie(name) {
  const escaped = name.replace(/[.*+?^${}()|[\]\\]/g, '\\$&');
  const match = document.cookie.match(new RegExp(`(?:^|; )${escaped}=([^;]*)`));
  return match ? decodeURIComponent(match[1]) : '';
}
function authHeaders() {
  const xsrf = readCookie('XSRF-TOKEN');
  return xsrf ? { 'X-XSRF-TOKEN': xsrf } : {};
}
async function jsonFetch(url, options = {}, isCurrent = () => true) {
  const epoch = state.sessionGeneration;
  const mutation = options.method && options.method !== 'GET';
  let response, text;
  try {
    response = await fetch(url, {
      ...options, credentials: 'same-origin',
      headers: { 'Content-Type': 'application/json', ...authHeaders(), ...(options.headers || {}) },
    });
    text = await response.text();
  } catch (_error) {
    if (epoch !== state.sessionGeneration || !isCurrent()) throw staleError();
    throw new Error(mutation ? 'Could not reach the server. Check refreshed data before submitting again.' : 'Could not reach the server. Retry when the connection is available.');
  }
  if (epoch !== state.sessionGeneration || !isCurrent()) throw staleError();
  if (!response.ok) {
    if (response.status === 401 && !url.includes('/session/login')) {
      const expired = Boolean(state.adminUsername);
      clearSession(expired ? 'Your session expired. Sign in again.' : '');
      throw staleError();
    }
    throw Object.assign(new Error(text || `${response.status} ${response.statusText}`), { status: response.status });
  }
  if (!text) return null;
  try { return JSON.parse(text); }
  catch (_error) { throw new Error('The server returned an invalid response format.'); }
}
function buildPageUrl(base, params) {
  const url = new URL(base, window.location.origin);
  if (params.query) url.searchParams.set('query', params.query);
  url.searchParams.set('offset', String(params.offset)); url.searchParams.set('limit', String(params.limit));
  url.searchParams.set('sort', params.sort);
  return url.toString();
}
function clientPage(items, params, choiceFn = userChoice) {
  const query = (params.query || '').trim().toLocaleLowerCase();
  const choices = items.map((item) => ({ item, choice: choiceFn(item) }));
  const normalized = !choices.length || typeof choices[0].choice === 'object';
  const filtered = choices.filter(({ item, choice }) => normalized
    ? item.username !== undefined ? matchesUser(item, query) : `${choice.primary} ${choice.secondary}`.toLocaleLowerCase().includes(query)
    : String(choice).toLocaleLowerCase().includes(query));
  const title = ({ choice }) => String(normalized ? choice.primary : choice).toLocaleLowerCase();
  filtered.sort((a, b) => title(a).localeCompare(title(b)));
  if ((params.sort || '').startsWith('-')) filtered.reverse();
  const total = filtered.length; const limit = params.limit || 20;
  const offset = Math.min(params.offset || 0, Math.max(0, Math.ceil(total / limit) - 1) * limit);
  if (!normalized) params.offset = offset;
  return { items: filtered.slice(offset, offset + limit).map(({ item, choice }) => normalized ? choice : item), pageInfo: {
    offset, limit, total, hasPrev: offset > 0, hasNext: offset + limit < total,
  } };
}
function clearSession(message = '') {
  state.sessionGeneration++; state.generation++; state.navigationGeneration++;
  closeInvitation(false);
  state.adminUsername = ''; state.group = null; state.lists = newLists();
  state.recordView = null; state.reportView = null;
  if (state.confirmation) finishConfirmation(false);
  closeEditor(false); $('#login-form').reset(); disposePickersWithin($('#page')); $('#page').replaceChildren(); $('#success-status').textContent = '';
  $('#page-error').textContent = ''; $('#page').removeAttribute('aria-busy');
  $('#logout-retry').classList.add('hidden');
  $('#login-error').textContent = message; $('#session-retry').classList.add('hidden'); renderShell();
}
function renderShell() {
  const authenticated = Boolean(state.adminUsername);
  $('#login-view').classList.toggle('hidden', authenticated);
  $('#dashboard-view').classList.toggle('hidden', !authenticated);
  $('#admin-identity').textContent = authenticated ? `Signed in as ${state.adminUsername}` : '';
  document.querySelectorAll('.nav a').forEach((element) => {
    const active = element.hash === `#/${state.route.groupId ? 'groups' : state.route.page}`;
    element.classList.toggle('active', active);
    if (active) element.setAttribute('aria-current', 'page'); else element.removeAttribute('aria-current');
  });
}
function announce(message) { $('#success-status').textContent = message; }
function parseRoute(hash) {
  const [pathname, search = ''] = hash.split('?');
  if (/^#\/(overview|users|admins|groups|authentication)$/.test(pathname)) return { path: pathname, page: pathname.slice(2), groupId: null };
  const userMatch = /^#\/users\/([^/]+)$/.exec(pathname);
  if (userMatch) {
    try { return { path: pathname, page: 'users', userId: decodeURIComponent(userMatch[1]), groupId: null }; }
    catch (_error) { return { path: '#/users', page: 'users', groupId: null }; }
  }
  const match = /^#\/groups\/([^/]+)\/(records|receipts|balances|statistics|settings)$/.exec(pathname);
  if (match) {
    try {
      const statistics = match[2] === 'statistics' ? Statistics.parse(new URLSearchParams(search)) : null;
      return { path: statistics ? `${pathname}?${statistics.error ? search : Statistics.params(statistics.query)}` : pathname, page: match[2], groupId: decodeURIComponent(match[1]), statistics };
    } catch (_error) { /* Invalid URL is Overview. */ }
  }
  return { path: '#/overview', page: 'overview', groupId: null };
}
function groupPath(groupId, section = 'records', query = null) {
  const base = `#/groups/${encodeURIComponent(groupId)}/${section}`;
  if (section !== 'statistics') return base;
  const filters = { ...(query || (state.route.page === 'statistics' ? state.route.statistics.query : Statistics.defaults())) };
  if (groupId !== state.route.groupId) { delete filters.userId; delete filters.currency; }
  return `${base}?${Statistics.params(filters)}`;
}
function groupApi(groupId) { return `/admin/groups/${encodeURIComponent(groupId)}`; }
async function routeChanged() {
  closeInvitation(false);
  const route = parseRoute(location.hash);
  const navigation = ++state.navigationGeneration;
  if (state.editor) {
    history.replaceState(null, '', state.route.path);
    if (!await requestDiscard(false) || navigation !== state.navigationGeneration) return;
  }
  history.replaceState(null, '', route.path);
  if (route.groupId !== state.route.groupId) {
    state.group = null;
    state.recordView = null;
    state.reportView = null;
    for (const kind of ['records', 'receipts']) state.lists[kind] = newLists()[kind];
  }
  state.route = route; renderShell();
  if (state.adminUsername) await loadPage();
}
function pageHeader(title, subtitle, actions = []) {
  const header = node('header', null, 'page-header');
  append(header, append(node('div'), node('h1', title), subtitle && node('p', subtitle, 'muted')), append(node('div', null, 'row-actions'), ...actions));
  return header;
}
function panelError(root, error, retry) {
  root.replaceChildren(node('p', error.status === 404 ? 'This item was not found or has been deleted.' : errorMessage(error), 'error'));
  if (error.status === 404 && state.route.groupId) root.append(link('Back to groups', '#/groups'));
  else root.append(button('Retry', retry));
}
async function loadPage() {
  $('#page-error').textContent = '';
  const request = ++state.generation; const epoch = state.sessionGeneration; const route = { ...state.route };
  const current = () => request === state.generation && epoch === state.sessionGeneration && Boolean(state.adminUsername);
  const fetchPage = (url) => jsonFetch(url, {}, current);
  const root = $('#page');
  const protectedRecords = route.page === 'records' && state.recordView?.groupId === route.groupId && state.recordView.surface?.isConnected;
  if (!protectedRecords) { disposePickersWithin(root); root.replaceChildren(node('p', 'Loading…', 'muted')); }
  root.setAttribute('aria-busy', 'true');
  try {
    if (route.page === 'authentication') {
      const settings = await fetchPage('/admin/authentication');
      if (current()) renderAuthentication(root, settings);
      return;
    }
    if (route.userId) {
      const details = await fetchPage(userApi(route.userId));
      if (current()) renderUserDetails(root, details);
      return;
    }
    if (route.groupId) {
      const base = groupApi(route.groupId);
      if (route.page === 'records') {
        await loadRecordPage(root, route.groupId, fetchPage, current);
        return;
      }
      const group = await fetchPage(base);
      if (!current()) return;
      state.group = group;
      if (route.page === 'statistics') {
        const surface = renderGroup(root, group, route.page, null);
        if (!route.statistics.error) {
          try {
            const query = new URLSearchParams({ from: route.statistics.query.from, to: route.statistics.query.to, bucket: route.statistics.query.bucket });
            if (route.statistics.query.userId) query.set('userId', route.statistics.query.userId);
            const data = await fetchPage(`${base}/statistics?${query}`);
            if (current()) surface.resolve(data);
          } catch (error) { if (current() && !error.stale) surface.reject(error); }
        }
      } else {
        let data = null;
        if (route.page !== 'settings') data = await fetchPage(`${base}/${route.page === 'balances' ? 'report' : route.page}`);
        if (!current()) return;
        renderGroup(root, group, route.page, data);
      }
    } else if (route.page === 'overview') {
      const counts = await Promise.all(['users', 'admins', 'groups'].map((kind) => fetchPage(buildPageUrl(`/admin/${kind}`, { query: '', offset: 0, limit: 1, sort: kind === 'groups' ? 'groupName' : 'username' }))));
      if (!current()) return;
      root.replaceChildren(pageHeader('Overview', null));
      const cards = node('div', null, 'stats-grid');
      ['users', 'admins', 'groups'].forEach((kind, index) => cards.append(append(link(capitalize(kind), `#/${kind}`, 'card stat-card'), node('strong', counts[index].pageInfo.total))));
      root.append(cards);
    } else {
      const kind = route.page; const params = { ...state.lists[kind] };
      let page = await fetchPage(buildPageUrl(`/admin/${kind}`, params));
      if (!current()) return;
      const last = Math.max(0, Math.ceil(page.pageInfo.total / params.limit) - 1) * params.limit;
      if (params.offset > last) {
        state.lists[kind].offset = last; params.offset = last;
        page = await fetchPage(buildPageUrl(`/admin/${kind}`, params));
      }
      if (current()) renderTopList(root, kind, page);
    }
  } catch (error) {
    if (current() && !error.stale) { state.group = null; panelError(root, error, loadPage); }
  } finally { if (current()) root.removeAttribute('aria-busy'); }
}
function capitalize(value) { return value.charAt(0).toUpperCase() + value.slice(1); }
function renderToolbar(kind, createActions = []) {
  const params = state.lists[kind]; const form = node('form', null, 'toolbar');
  const search = node('input'); search.type = 'search'; search.value = params.query;
  const sort = node('select');
  const label = ({ records: 'Title', receipts: 'Note', groups: 'Group name' })[kind] || 'Username';
  const field = params.sort.replace(/^-/, '');
  for (const [value, text] of [[field, `${label} A–Z`], [`-${field}`, `${label} Z–A`]]) {
    const option = node('option', text); option.value = value; sort.append(option);
  }
  sort.value = params.sort;
  const apply = node('button', 'Apply', 'secondary'); apply.type = 'submit';
  append(form, labeled(`Search ${kind}`, search), labeled('Sort', sort), apply, append(node('div', null, 'row-actions'), ...createActions));
  form.onsubmit = guarded(async (event) => {
    event.preventDefault(); params.query = search.value; params.sort = sort.value; params.offset = 0; await loadPage();
  });
  return form;
}
function labeled(label, control) { return append(node('label'), node('span', label), control); }
function field(label, value) { return append(node('div', null, 'entity-field'), node('span', label, 'field-label'), value instanceof Node ? value : node('span', value)); }
function entity(fields, actions = []) {
  return append(node('article', null, 'entity-card'), ...fields.map(([label, value]) => field(label, value)), actions.length && field('Actions', append(node('div', null, 'row-actions'), ...actions)));
}
function renderPager(page, kind) {
  const info = page.pageInfo; const pager = node('div', null, 'pager');
  const move = async offset => {
    if (kind === 'records') Object.assign(state.lists.records, state.recordView.applied);
    state.lists[kind].offset = offset; await loadPage();
  };
  const previous = button('Previous', () => move(Math.max(0, info.offset - info.limit)));
  const next = button('Next', () => move(info.offset + info.limit));
  previous.disabled = !info.hasPrev; next.disabled = !info.hasNext;
  append(pager, previous, node('span', `${info.total ? info.offset + 1 : 0}–${Math.min(info.offset + info.limit, info.total)} of ${info.total}`), next);
  return pager;
}
function userApi(id) { return `/admin/users/${encodeURIComponent(id)}`; }
function userPath(id) { return `#/users/${encodeURIComponent(id)}`; }
function userStatus(user) { return !user.enabled ? 'Disabled' : !user.hasPassword && !(user.hasIdentity ?? Boolean(user.identity)) ? 'Pending sign-in setup' : 'Enabled'; }
function userMethods(user) {
  const identity = user.hasIdentity ?? Boolean(user.identity);
  return user.hasPassword ? identity ? 'Both (Local and SSO)' : 'Local' : identity ? 'SSO' : 'None';
}
function checkboxField(parent, title, draft, key) {
  const input = node('input'); input.type = 'checkbox'; input.checked = draft[key];
  input.onchange = () => { draft[key] = input.checked; markDirty(); };
  parent.append(labeled(title, input)); return input;
}
function authenticationPayload(draft) {
  if (!draft.providerName.trim() || draft.providerName.length > 100 || /[\u0000-\u001f\u007f-\u009f]/u.test(draft.providerName)) throw new Error('Provider name must be 1–100 non-control characters.');
  if (draft.clientSecret && !draft.clientSecret.trim()) throw new Error('Client secret cannot be blank.');
  return { revision: draft.revision, localRegistrationEnabled: draft.localRegistrationEnabled, ssoRegistrationEnabled: draft.ssoRegistrationEnabled, oidcEnabled: draft.oidcEnabled, providerName: draft.providerName, issuer: draft.issuer.trim() || null, clientId: draft.clientId.trim() || null, clientSecret: draft.clientSecret || null };
}
function authenticationFields(settings) {
  return [['Revision', settings.revision], ['Configuration source', settings.environmentManaged ? 'Environment variables' : 'Database'], ['Local registration', settings.localRegistrationEnabled ? 'Enabled' : 'Disabled'], ['SSO registration', settings.ssoRegistrationEnabled ? 'Enabled' : 'Disabled'], ['SSO provider', settings.oidcEnabled ? 'Enabled' : 'Disabled'], ['Provider name', settings.providerName], ['Issuer', settings.issuer || 'Not configured'], ['Client ID', settings.clientId || 'Not configured'], ['Client secret', settings.hasClientSecret ? 'Configured' : 'Not configured'], ['Runtime', settings.runtimeReady ? 'Ready' : settings.runtimeError || 'Not ready'], ['Enabled SSO-only users', settings.ssoOnlyUserCount], ['Callback URL', settings.callbackUrl || 'Unavailable'], ['Back-channel logout URL', settings.backchannelLogoutUrl || 'Unavailable'], ['Post-logout redirect URL', settings.postLogoutRedirectUrl || 'Unavailable']];
}
function renderAuthentication(root, settings) {
  root.replaceChildren(pageHeader('Authentication', 'Application sign-in settings. Operator sign-in remains separate.', settings.environmentManaged ? [] : [button('Edit authentication', () => openEditor('authentication', settings), 'primary')]));
  root.append(entity(authenticationFields(settings)));
  if (settings.environmentManaged) root.append(node('p', 'Change deployment environment variables, then restart the server.', 'muted'));
  root.append(node('p', 'Registration switches affect public new-account creation only. Known-account sign-in, explicit linking and operator provisioning remain available. No account is linked automatically by email.', 'muted'));
  if (!settings.environmentManaged) root.append(button('Clear provider connection', () => operatorAction(root, 'Clear provider connection?', 'This disables SSO and removes the saved issuer, client ID and secret. All identities must first be unlinked. Pending SSO flows and invitations are invalidated; password sign-in is preserved.', `/admin/authentication/provider?revision=${encodeURIComponent(settings.revision)}`, 'DELETE'), 'danger'));
}
function renderAuthenticationEditor(body, draft, settings) {
  checkboxField(body, 'Allow local registration', draft, 'localRegistrationEnabled');
  checkboxField(body, 'Allow SSO registration', draft, 'ssoRegistrationEnabled');
  checkboxField(body, 'Enable SSO provider', draft, 'oidcEnabled');
  inputField(body, 'Provider name', draft, 'providerName', { required: true, maxLength: 100 });
  inputField(body, 'Issuer', draft, 'issuer', { type: 'url', autocomplete: 'off' });
  inputField(body, 'Client ID', draft, 'clientId', { autocomplete: 'off' });
  inputField(body, 'Client secret (leave blank to keep saved secret)', draft, 'clientSecret', { type: 'password', autocomplete: 'new-password' });
  body.append(node('p', settings.hasClientSecret ? 'Saved secret configured.' : 'No saved secret.', 'muted'));
  body.append(entity(authenticationFields(settings).filter(([label]) => ['Runtime', 'Enabled SSO-only users', 'Callback URL', 'Back-channel logout URL', 'Post-logout redirect URL'].includes(label))));
  body.append(node('p', 'Every saved revision invalidates pending SSO flows and SSO invitations, not password-reset invitations. Disabling also revokes SSO sessions; password sessions remain active.', 'muted'));
  const result = node('div', null, 'stack'); result.setAttribute('aria-live', 'polite');
  const test = button('Test unsaved connection', async () => {
    const editor = state.editor;
    if (!editor || editor.pending) return;
    const payload = authenticationPayload(draft);
    editor.pending = true; setEditorPending(true); $('#editor-error').textContent = ''; result.replaceChildren();
    try {
      const tested = await jsonFetch('/admin/authentication/test', { method: 'POST', body: JSON.stringify(payload) }, () => state.editor === editor);
      result.append(entity([['Issuer', tested.issuer], ['Authorization endpoint', tested.authorizationEndpoint], ['Token endpoint', tested.tokenEndpoint], ['JWKS URI', tested.jwksUri], ['UserInfo endpoint', tested.userinfoEndpoint || 'Not advertised'], ['End-session endpoint', tested.endSessionEndpoint || 'Not advertised'], ['PKCE', tested.pkceSupported ? 'Supported' : 'Not supported'], ['Back-channel logout', tested.backchannelLogoutSupported ? 'Supported' : 'Not advertised']]));
      result.append(node('p', 'Discovery and signing keys validated. Not saved. Sign in to verify client-secret authentication.', 'muted'));
    } finally { if (state.editor === editor) { editor.pending = false; setEditorPending(false); } }
  });
  body.append(test, result);
}
async function operatorAction(root, title, impact, url, method, invitation = false) {
  if (root.dataset.mutationPending || state.editor || state.confirmation) return;
  const generation = state.generation; const epoch = state.sessionGeneration;
  const current = () => root.isConnected && generation === state.generation && epoch === state.sessionGeneration && Boolean(state.adminUsername);
  root.dataset.mutationPending = 'true';
  const controls = Array.from(root.querySelectorAll('button')); const disabled = controls.map(control => control.disabled);
  controls.forEach(control => { control.disabled = true; });
  let response;
  try {
    const accepted = await confirmAction(title, impact, async () => {
      if (!current()) throw staleError();
      response = await jsonFetch(url, { method }, current);
    }, invitation ? 'Generate invitation' : 'Confirm');
    if (!accepted || !current()) return;
    announce(invitation ? 'Invitation generated. Copy it now; it cannot be retrieved later.' : 'Changes saved.');
    await loadPage();
    if (invitation && epoch === state.sessionGeneration && state.adminUsername && generation + 1 === state.generation) showInvitation(response);
  } finally {
    delete root.dataset.mutationPending;
    controls.forEach((control, index) => { if (control.isConnected) control.disabled = disabled[index]; });
  }
}
function renderUserDetails(root, details) {
  const profile = details.profile; const user = profile.user; const base = userApi(user.userId);
  root.replaceChildren(link('Back to users', '#/users'), pageHeader(userLabel(user), null, [button('Edit profile', () => openEditor('user-profile', details), 'primary')]));
  root.append(entity([['User ID', node('span', user.userId, 'mono')], ['Email', profile.email], ['Email verification', profile.emailVerified ? 'Verified' : 'Unverified'], ['Status', userStatus(details)], ['Sign-in methods', userMethods(details)], ['Password', details.hasPassword ? 'Configured' : 'Not configured']]));
  const identity = details.identity;
  root.append(node('h2', 'SSO identity'));
  root.append(identity ? entity([['Provider', identity.providerName], ['Issuer', node('span', identity.issuer, 'mono')], ['Subject', node('span', identity.subject, 'mono')], ['Claimed email', identity.claimedEmail || 'Not supplied'], ['Claimed email verification', identity.claimedEmailVerified ? 'Verified by provider' : 'Unverified'], ['Linked at', identity.linkedAt], ['Last SSO login', identity.lastLoginAt || 'Never']]) : node('p', 'No SSO identity linked.', 'muted'));
  const actions = node('div', null, 'row-actions');
  actions.append(button('Generate password reset invitation', () => operatorAction(root, 'Generate password reset invitation?', 'This replaces any unused password-reset invitation. Possession allows a password reset, including for a disabled account, but does not enable it. Issuance does not change credentials or revoke sessions; successful redemption does.', `${base}/password-reset`, 'POST', true)));
  if (!identity) actions.append(button('Generate SSO linking invitation', () => operatorAction(root, 'Generate SSO linking invitation?', 'The enabled target must have no SSO identity and the provider must be enabled and ready. This replaces the previous unused SSO invitation. Whoever possesses the link can authenticate with the provider and explicitly bind that identity to this exact account.', `${base}/identity-invitation`, 'POST', true)));
  if (details.hasPassword) actions.append(button('Remove password', () => operatorAction(root, 'Remove password?', 'All sessions and outstanding sign-in grants will be revoked. An enabled account must retain a viable configured SSO identity. A disabled account may lose its last sign-in method.', `${base}/password`, 'DELETE'), 'danger'));
  if (identity) actions.append(button('Remove SSO identity', () => operatorAction(root, 'Remove SSO identity?', 'The identity will be unlinked and all sessions and outstanding sign-in grants revoked. An enabled account must retain a local password. Ledger history is preserved.', `${base}/identity`, 'DELETE'), 'danger'));
  actions.append(button('Revoke all sessions', () => operatorAction(root, 'Revoke all sessions?', 'All application sessions and account-bound pending sign-in flows will be revoked. Credentials are unchanged and a fresh sign-in remains possible.', `${base}/revoke-sessions`, 'POST'), 'danger'));
  root.append(actions, node('h2', 'Pending invitations'));
  const invitations = node('div', null, 'entity-list');
  for (const invitation of details.invitations) invitations.append(entity([['Purpose', invitation.purpose === 'password_reset' ? 'Password reset' : 'SSO linking'], ['Expires at', invitation.expiresAt]], [button('Revoke invitation', () => operatorAction(root, 'Revoke invitation?', 'The unused invitation and any bound SSO flows will stop working. Credentials are unchanged.', `${base}/invitations/${encodeURIComponent(invitation.invitationId)}`, 'DELETE'), 'danger', `revoke-${invitation.invitationId}`)]));
  if (!details.invitations.length) invitations.append(node('p', 'No pending invitations.', 'muted'));
  root.append(invitations);
}
let invitationFocus = null;
function closeInvitation(restore = true) {
  $('#invitation-body').querySelectorAll('input').forEach(input => { input.value = ''; });
  const reference = invitationFocus; invitationFocus = null;
  const dialog = $('#invitation-dialog');
  if (dialog.open) dialog.close();
  $('#invitation-body').replaceChildren();
  if (restore) restoreFocus(reference);
}
function showInvitation(issued) {
  closeInvitation(false); invitationFocus = focusReference();
  const body = $('#invitation-body');
  body.append(node('p', 'Copy this link now; it will not be shown again. Anyone with it can complete sign-in setup. Share it privately with the intended recipient.'));
  const input = node('input'); input.type = 'text'; input.readOnly = true; input.value = issued.url; input.autocomplete = 'off'; input.spellcheck = false;
  body.append(labeled('Invitation URL', input), node('p', `Expires at: ${issued.expiresAt}`));
  const status = node('p', '', 'muted'); status.setAttribute('role', 'status');
  body.append(button('Copy invitation', async () => {
    const epoch = state.sessionGeneration;
    try {
      if (!navigator.clipboard?.writeText) throw new Error('Clipboard unavailable');
      await navigator.clipboard.writeText(input.value);
      if (epoch === state.sessionGeneration && input.isConnected) status.textContent = 'Copied. Share privately with the intended recipient.';
    } catch (_error) { if (input.isConnected) { status.textContent = 'Select and copy the link manually.'; input.focus(); input.select(); } }
  }), status);
  $('#invitation-dialog').showModal(); input.focus(); input.select();
}
$('#invitation-close').onclick = () => closeInvitation();
$('#invitation-dialog').addEventListener('cancel', event => { event.preventDefault(); closeInvitation(); });
$('#invitation-dialog').addEventListener('close', () => { if (!$('#invitation-dialog').open) $('#invitation-body').replaceChildren(); });

function renderTopList(root, kind, page) {
  const singular = ({ users: 'user', admins: 'admin', groups: 'group' })[kind];
  root.replaceChildren(pageHeader(capitalize(kind), null));
  root.append(renderToolbar(kind, [button(`Create ${singular}`, () => openEditor(singular), 'primary', `create-${singular}`)]));
  const list = node('div', null, 'entity-list');
  for (const item of page.items) {
    if (kind === 'groups') list.append(entity([['Group', item.groupName], ['Currency policy', groupPolicy(item)], ['Owner', userLabel(item.owner)], ['Active members', item.members.length], ['ID', item.groupId.slice(0, 8)]], [link('Open group', groupPath(item.groupId))]));
    else {
      const profile = kind === 'users' ? item.profile : null;
      const user = profile?.user;
      const name = user ? userLabel(user) : item.summaryUsername;
      list.append(entity([['Identity', name], ['ID', node('span', user?.userId || item.adminId, 'mono')], ...(user ? [['Email', `${profile.email} · ${profile.emailVerified ? 'Verified' : 'Unverified'}`], ['Status', userStatus(item)], ['Sign-in methods', userMethods(item)]] : [])], user ? [link('Manage user', userPath(user.userId)), button('Delete', () => deleteUser(user), 'danger', `delete-user-${user.userId}`)] : []));
    }
  }
  if (!page.items.length) list.append(node('p', 'No matching items. Change the search or create one.', 'empty-state'));
  root.append(list, renderPager(page, kind));
}
function renderGroup(root, group, section, data) {
  root.replaceChildren(link('Back to groups', '#/groups'), pageHeader(group.groupName, `${groupPolicy(group)} · Owner: ${userLabel(group.owner)} · ${group.members.length} active members`, [button('Import CSV', () => openEditor('csv'), 'secondary', 'import-csv')]));
  const groupChoice = (value) => ({ id: value.groupId, primary: value.groupName, secondary: `${userLabel(value.owner)} · ${value.members.length} members · ${value.groupId.slice(0, 8)}`, entity: value });
  const epoch = state.sessionGeneration; let pickerRequest = 0;
  const switcher = entityPicker({
    label: 'Switch group', getSelected: () => groupChoice(group),
    loadPage: async ({ query, offset, limit, isCurrent }) => {
      const request = ++pickerRequest;
      const page = await jsonFetch(buildPageUrl('/admin/groups', { query, offset, limit, sort: 'groupName' }), {}, () => (!isCurrent || isCurrent()) && root.isConnected && epoch === state.sessionGeneration && state.route.groupId === group.groupId && request === pickerRequest);
      return { items: page.items.map(groupChoice), pageInfo: page.pageInfo };
    },
    onSelect: (choice) => { if (choice.id !== group.groupId) location.hash = groupPath(choice.id, section); },
  });
  root.append(switcher.element);
  const tabs = node('nav', null, 'group-tabs'); tabs.setAttribute('aria-label', 'Group sections');
  for (const [key, title] of [['records', 'Records'], ['receipts', 'Receipts'], ['balances', 'Balances'], ['statistics', 'Statistics'], ['settings', 'Members & settings']]) {
    const tab = link(title, groupPath(group.groupId, key));
    if (key === section) { tab.classList.add('active'); tab.setAttribute('aria-current', 'page'); }
    tabs.append(tab);
  }
  root.append(tabs);
  if (section === 'settings') return renderSettings(root, group);
  if (section === 'balances') return renderReport(root, group, data);
  if (section === 'statistics') return renderStatistics(root, group);
  if (section === 'records') return renderRecordSurface(root, group);
  const kind = section;
  const actions = [button('Add receipt', () => openEditor('receipt'), 'primary', 'add-receipt')];
  root.append(renderToolbar(kind, actions));
  const page = clientPage(data, state.lists[kind], (item) => item.note);
  const list = node('div', null, 'entity-list');
  for (const item of page.items) {
    {
      const details = node('details'); details.append(node('summary', `${item.records.length} expenses`));
      for (const expense of item.records) details.append(node('p', `${expense.title}: ${recordMoney(expense)} · ${userLabel(expense.paidBy)} · ${expense.date}`), recordDetails(expense));
      list.append(entity([['Receipt note', node('span', item.note || '(No note)', 'receipt-note')], ['Totals', receiptTotals(item.records)], ['Uploader', userLabel(item.uploadedBy)], ['Created', item.createdAt], ['Expenses', details]], [button('Edit', () => openEditor('receipt', item), 'secondary', `edit-receipt-${item.receiptId}`), button('Delete', () => deleteReceipt(group, item), 'danger', `delete-receipt-${item.receiptId}`)]));
    }
  }
  if (!page.items.length) list.append(node('p', 'No matching items. Use the actions above to add one.', 'empty-state'));
  root.append(list, renderPager(page, kind));
}
// Composition only: all query matching and financial comparisons belong to the server.
function recordQueryTokens(text) {
  const tokens = []; let i = 0;
  while (i < text.length) {
    if (/\s/.test(text[i])) { i++; continue; }
    const start = i; let value = '', qualifier = null, quoted = false, inside = false;
    while (i < text.length && (inside || !/\s/.test(text[i]))) {
      const ch = text[i++];
      if (ch === '"') { inside = !inside; quoted = true; continue; }
      if (inside && ch === '\\') {
        const escaped = text[i++];
        if (escaped !== '"' && escaped !== '\\') throw new Error('Only escaped quotes and backslashes are allowed inside quotes.');
        value += escaped;
      } else if (!inside && ch === ':' && qualifier === null) { qualifier = value; value = ''; }
      else value += ch;
    }
    if (inside) throw new Error('Unterminated quoted term.');
    if (!value) throw new Error('Query terms and filter values cannot be empty.');
    tokens.push({ start, end: i, qualifier, value, quoted, raw: text.slice(start, i) });
  }
  return tokens;
}
function quoteRecordValue(value) { return `"${value.replace(/\\/g, '\\\\').replace(/"/g, '\\"')}"`; }
function rewriteRecordTokens(text, changes, addition = '') {
  for (const change of [...changes].sort((a, b) => b.token.start - a.token.start)) text = text.slice(0, change.token.start) + change.text + text.slice(change.token.end);
  return addition ? `${text}${text && !/\s$/.test(text) ? ' ' : ''}${addition}` : text;
}
async function loadRecordPage(root, groupId, fetchPage, current) {
  let view = state.recordView;
  if (!view || view.groupId !== groupId) view = state.recordView = { groupId, draft: state.lists.records.query, page: null, identities: null };
  try {
    if (!view.surface?.isConnected) {
      const group = await fetchPage(groupApi(groupId)); if (!current()) return;
      state.group = group; renderGroup(root, group, 'records', null);
    }
    view.status.textContent = view.page ? 'Loading · previous results shown.' : 'Loading…'; view.errorNode.textContent = '';
    const params = { ...state.lists.records };
    let page = await fetchPage(buildPageUrl(`${groupApi(groupId)}/records`, params));
    if (!current()) return;
    const limit = page.pageInfo.limit;
    const last = Math.max(0, Math.ceil(page.pageInfo.total / limit) - 1) * limit;
    if (params.offset > last) {
      params.offset = last; state.lists.records.offset = last;
      page = await fetchPage(buildPageUrl(`${groupApi(groupId)}/records`, params));
    }
    if (!current()) return;
    view.page = page; view.applied = params; renderRecordResults(view);
  } catch (error) {
    if (!current() || error.stale) return;
    if (!view.surface?.isConnected) throw error;
    view.errorNode.replaceChildren(node('span', errorMessage(error)), button('Retry search', loadPage));
    view.status.textContent = view.page ? 'Showing previous results.' : 'No results loaded.';
  }
}
function renderRecordResults(view) {
  const page = view.page; const group = state.group; const list = node('div', null, 'entity-list');
  view.status.textContent = view.draft === view.applied.query ? 'Results' : 'Previous results (search not applied)';
  for (const item of page.items) {
    const kind = recordKind(item);
    list.append(entity([['Title / type', `${item.title} · ${kind === 'transfer' ? 'Payment / Transfer' : capitalize(kind)}`], ['Amount', recordMoney(item)], ['Date', item.date], ['Created (UTC)', item.createdAt], [kind === 'income' ? 'Received by' : 'Payer', recordUserLabel(kind === 'income' ? item.receivedBy : item.paidBy)], ...(item.note?.trim() ? [['Record note', node('p', item.note, 'record-note record-note-preview')]] : []), ['Details', recordDetails(item)]], [button('Edit', () => openEditor(kind, item), 'secondary', `edit-record-${item.recordId}`), button('Delete', () => deleteRecord(group, item), 'danger', `delete-record-${item.recordId}`)]));
  }
  if (!page.items.length) list.append(node('p', 'No matching records.', 'empty-state'), button('Clear filters', view.clear));
  view.results.replaceChildren(list, renderPager(page, 'records'));
}
function renderRecordSurface(root, group) {
  const view = state.recordView; const surface = node('section', null, 'stack record-search'); view.surface = surface;
  view.identityChoices ||= new Map(group.members.map(user => [user.userId, userChoice(user)]));
  const toolbar = node('div', null, 'stack'); const search = node('input'); search.type = 'search'; search.value = view.draft;
  search.placeholder = 'title:"team lunch" member:@alice';
  const chips = node('div', null, 'record-query-chips row-actions'); const syntax = node('p', '', 'error'); syntax.setAttribute('role', 'alert');
  const editor = node('div', null, 'record-filter-editor stack'); editor.hidden = true;
  const queryError = node('div', null, 'error'); queryError.setAttribute('role', 'alert'); view.errorNode = queryError;
  const sort = node('select');
  for (const [key, title] of [['title', 'Title'], ['date', 'Date'], ['createdAt', 'Created time (UTC)'], ['type', 'Type'], ['payer', 'Payer'], ['payee', 'Payee'], ['currency', 'Currency'], ['amount', 'Amount (grouped by currency)']]) {
    const option = node('option', title); option.value = key; sort.append(option);
  }
  sort.value = state.lists.records.sort.replace(/^-/, '');
  const direction = node('select');
  for (const [value, text] of [['asc', 'Ascending'], ['desc', 'Descending']]) { const option = node('option', text); option.value = value; direction.append(option); }
  direction.value = state.lists.records.sort.startsWith('-') ? 'desc' : 'asc';
  const changeSort = guarded(async () => {
    state.lists.records.query = view.applied?.query || '';
    state.lists.records.sort = `${direction.value === 'desc' ? '-' : ''}${sort.value}`;
    state.lists.records.offset = 0;
    await loadPage();
  });
  sort.onchange = changeSort; direction.onchange = changeSort;
  const commit = async () => {
    view.draft = search.value;
    try { recordQueryTokens(view.draft); } catch (error) { syntax.textContent = errorMessage(error); view.status.textContent = view.page ? 'Previous results · invalid search.' : 'Invalid search.'; return; }
    state.lists.records.query = view.draft; state.lists.records.offset = 0; await loadPage();
  };
  view.clear = async () => { view.draft = ''; search.value = ''; state.lists.records.query = ''; state.lists.records.offset = 0; refreshChips(); await loadPage(); };
  search.oninput = () => { view.draft = search.value; refreshChips(); };
  search.onkeydown = guarded(async event => { if (event.key === 'Enter') { event.preventDefault(); await commit(); } });
  function setDraft(text) { view.draft = text; search.value = text; refreshChips(); }
  function refreshChips() {
    if (view.page) view.status.textContent = view.draft === view.applied.query ? 'Results' : 'Previous results (search not applied)';
    chips.replaceChildren(); syntax.textContent = '';
    try {
      for (const token of recordQueryTokens(view.draft)) {
        const chip = node('span', null, 'record-query-chip');
        const identity = ['payer', 'payee', 'member'].includes(token.qualifier) && view.identityChoices.get(token.value);
        const label = identity ? `${token.qualifier}: ${identity.primary} · ${identity.secondary}` : token.raw;
        chip.append(button(`Edit ${label}`, () => openFilter(token)), button(`Remove ${label}`, () => setDraft(rewriteRecordTokens(view.draft, [{ token, text: '' }])))); chips.append(chip);
      }
    } catch (error) { syntax.textContent = errorMessage(error); }
  }
  async function openFilter(token = null) {
    disposePickersWithin(editor); editor.replaceChildren(); editor.hidden = false;
    const fieldSelect = node('select');
    for (const [value, label] of [['', 'Text (any field)'], ['title', 'Title'], ['note', 'Note'], ['type', 'Type'], ['payer', 'Payer'], ['payee', 'Payee'], ['member', 'Member'], ['date', 'Date'], ['created', 'Created (UTC day)'], ['currency', 'Currency'], ['amount', 'Original amount']]) { const option = node('option', label); option.value = value; fieldSelect.append(option); }
    fieldSelect.value = token?.qualifier || '';
    const controls = node('div', null, 'stack'); editor.append(labeled('Filter field', fieldSelect), controls);
    const tokenDraft = view.draft;
    const current = () => editor.isConnected && !editor.hidden && state.recordView === view && state.route.groupId === group.groupId && view.draft === tokenDraft;
    async function fields() {
      disposePickersWithin(controls); controls.replaceChildren(); const key = fieldSelect.value;
      let selectedIdentity = token && ['payer', 'payee', 'member'].includes(key) ? view.identityChoices.get(token.value) || null : null;
      let selectedCurrency = null;
      const input = node('input'); input.value = token?.value || '';
      const message = node('p', '', 'error'); message.setAttribute('role', 'alert');
      const operator = node('select');
      for (const op of ['=', '<', '<=', '>', '>=', '..']) { const option = node('option', op === '..' ? 'Inclusive range' : op); option.value = op; operator.append(option); }
      const upper = node('input'); const comparison = ['date', 'created', 'amount'].includes(key);
      if (comparison) {
        const raw = token?.qualifier === key ? token.value : '';
        const range = raw.split('..'); const match = /^(<=|>=|=|<|>)(.*)$/.exec(raw);
        operator.value = range.length === 2 ? '..' : match?.[1] || '=';
        input.value = range.length === 2 ? range[0] : match?.[2] || raw; upper.value = range[1] || '';
        input.type = upper.type = key === 'amount' ? 'text' : 'date';
        if (key === 'amount') { input.inputMode = upper.inputMode = 'decimal'; }
        const rangeLabel = labeled('Upper bound (inclusive)', upper); rangeLabel.hidden = operator.value !== '..';
        operator.onchange = () => { rangeLabel.hidden = operator.value !== '..'; };
        controls.append(labeled('Comparison', operator), labeled(key === 'created' ? 'UTC calendar day / lower bound' : 'Value / lower bound', input), rangeLabel);
      }
      const type = node('select');
      if (key === 'type') {
        for (const [value, label] of [['expense', 'Expense'], ['income', 'Income'], ['transfer', 'Payment / Transfer']]) { const option = node('option', label); option.value = value; type.append(option); }
        type.value = token?.value.toLowerCase() || 'expense'; controls.append(labeled('Record type', type));
      } else if (['payer', 'payee', 'member'].includes(key)) {
        const picker = entityPicker({ label: 'Member', getSelected: () => selectedIdentity,
          loadPage: async ({ query, offset, limit, isCurrent }) => {
            if (!view.identities) {
              const report = await jsonFetch(`${groupApi(group.groupId)}/report`, {}, () => current() && isCurrent());
              const users = new Map(group.members.map(user => [user.userId, user]));
              for (const section of report.currencies) for (const row of section.balances) users.set(row.user.userId, row.user);
              view.identities = [...users.values()];
            }
            if (!current() || !isCurrent()) throw staleError();
            const active = new Set(group.members.map(user => user.userId));
            const names = new Map(); for (const user of view.identities) { const name = user.displayName.toLowerCase(); names.set(name, (names.get(name) || 0) + 1); }
            return clientPage(view.identities.filter(user => matchesUser(user, query)), { query: '', offset, limit, sort: 'username' }, user => ({ ...userChoice(user), secondary: `${user.displayName}${active.has(user.userId) ? '' : ' · Former member'}${names.get(user.displayName.toLowerCase()) > 1 ? ` · ${user.userId}` : ''}` }));
          }, onSelect: choice => { selectedIdentity = choice; view.identityChoices.set(choice.id, choice); } });
        controls.append(picker.element);
        if (token) controls.append(node('p', `Current filter: ${token.raw}`, 'muted'));
      } else if (!comparison && key !== 'currency') controls.append(labeled('Text', input));
      const replace = node('input'); replace.type = 'checkbox'; let currencyTokens = [];
      if (key === 'currency' || key === 'amount') {
        try { currencyTokens = recordQueryTokens(view.draft).filter(t => t.qualifier === 'currency'); } catch (_) { /* Keep malformed text editable. */ }
        const picker = entityPicker({ label: 'Currency (required)', getSelected: () => selectedCurrency,
          loadPage: async ({ query, offset, limit, isCurrent }) => {
            const catalog = await getCurrencies(() => current() && isCurrent());
            return clientPage(catalog, { query, offset, limit, sort: 'code' }, currency => ({ id: currency.code, primary: currency.code, secondary: currency.name, entity: currency }));
          }, onSelect: choice => { selectedCurrency = choice; } });
        controls.append(picker.element);
        if (key === 'amount') controls.append(labeled('Explicitly replace conflicting currency filters', replace));
      }
      controls.append(message, append(node('div', null, 'row-actions'), button(token ? 'Update draft filter' : 'Add to draft', () => {
        try {
          if (!current()) throw new Error('The draft changed. Reopen this filter to edit it.');
          let value, literal = false;
          if (key === 'type') value = type.value;
          else if (['payer', 'payee', 'member'].includes(key)) { if (!selectedIdentity && !token) throw new Error('Choose a public identity.'); value = selectedIdentity?.id || token.value; literal = !selectedIdentity && token.quoted; }
          else if (key === 'currency') { if (!selectedCurrency) throw new Error('Choose a currency.'); value = selectedCurrency.id; }
          else if (comparison) { if (!input.value || (operator.value === '..' && !upper.value)) throw new Error('Enter both bounds for a range, or a comparison value.'); value = operator.value === '..' ? `${input.value}..${upper.value}` : `${operator.value}${input.value}`; }
          else { if (!input.value) throw new Error('Enter a nonempty value.'); value = input.value; literal = true; }
          const serialized = `${key ? `${key}:` : ''}${literal ? quoteRecordValue(value) : value}`;
          const changes = token ? [{ token, text: serialized }] : []; let addition = token ? '' : serialized;
          if (key === 'amount') {
            if (!selectedCurrency) throw new Error('Choose the original record currency for this amount.');
            const conflicting = currencyTokens.filter(t => t.value.toUpperCase() !== selectedCurrency.id);
            if (conflicting.length && !replace.checked) throw new Error('Confirm replacement of conflicting currency filters, or choose their currency.');
            if (conflicting.length) for (const t of currencyTokens) changes.push({ token: t, text: '' });
            if (conflicting.length || !currencyTokens.length) addition = `${addition}${addition ? ' ' : ''}currency:${selectedCurrency.id}`;
          }
          setDraft(rewriteRecordTokens(view.draft, changes, addition)); disposePickersWithin(editor); editor.hidden = true; search.focus();
        } catch (error) { message.textContent = errorMessage(error); }
      }), button('Cancel filter', () => { disposePickersWithin(editor); editor.hidden = true; search.focus(); })));
    }
    fieldSelect.onchange = guarded(fields); await fields();
  }
  toolbar.append(labeled('Search records', search), append(node('div', null, 'row-actions'), button('Search', commit), button('Clear', view.clear), button('Add filter', () => openFilter())), syntax, chips, editor, queryError, append(node('div', null, 'row-actions'), labeled('Sort records', sort), labeled('Direction', direction)), append(node('div', null, 'row-actions'), button('Add expense', () => openEditor('expense'), 'primary', 'add-expense'), button('New income', () => openEditor('income'), 'secondary', 'add-income'), button('Add transfer', () => openEditor('transfer'), 'secondary', 'add-transfer')));
  view.status = node('p', '', 'muted'); view.status.setAttribute('role', 'status'); view.results = node('div', null, 'stack');
  surface.append(toolbar, view.status, view.results); root.append(surface); refreshChips();
  if (view.page) renderRecordResults(view);
  return view;
}

const M = globalThis.JizhangMoney;
const minor = (value) => M.parseMinorAmount(value, { allowNegative: true });
const money = (value, currency) => M.formatMoney({ code: currency.code, minorDigits: currency.minorUnits, amountMinor: typeof value === 'bigint' ? value : minor(value) });
const major = (value, currency) => M.formatMajorAmount(typeof value === 'bigint' ? value : minor(value), { minorDigits: currency.minorUnits });
const groupPolicy = (group) => group.currencyMode === 'single' ? `${group.defaultCurrency.code} settlement` : `Multiple currencies · default ${group.defaultCurrency.code}`;
function recordKind(record) {
  switch (record.tag) {
    case 'ExpenseRecord': return 'expense';
    case 'IncomeRecord': return 'income';
    case 'TransferRecord': return 'transfer';
    default: throw new Error(`Unknown record kind: ${record.tag}`);
  }
}
function recordUserLabel(user) { return `${userLabel(user)}${state.group.members.some(member => member.userId === user.userId) ? '' : ' · Former member'}`; }
function recordMoney(record) { return money(record.amountMinor, record.currency) + (record.exchangeRate ? ` · ${money(record.settlementAmountMinor, record.settlementCurrency)} settlement` : ''); }
function rateDescription(rate) {
  if (!rate) return '';
  if (rate.source === 'manual') return `Manual rate ${rate.rate}`;
  const stale = Date.parse(`${new Date().toISOString().slice(0, 10)}T00:00:00Z`) - Date.parse(`${rate.rateDate}T00:00:00Z`) > 7 * 86400000;
  return `Frankfurter daily reference ${rate.rate} · effective ${rate.rateDate} · fetched ${rate.fetchedAt}${stale ? ' · Warning: reference is more than seven days old' : ''}`;
}
function recordDetails(record) {
  const details = node('details'); details.append(node('summary', 'Details'));
  const kind = recordKind(record);
  if (record.note?.trim()) details.append(node('h4', 'Record note'), node('p', record.note, 'record-note record-note-full'));
  details.append(node('p', `${kind === 'income' ? 'Received by' : 'Paid by'}: ${recordUserLabel(kind === 'income' ? record.receivedBy : record.paidBy)}`));
  if (kind === 'transfer') details.append(node('p', `To: ${recordUserLabel(record.transferTo)}`));
  else {
    details.append(node('p', `Split method: ${record.splitMode}`), node('h4', kind === 'income' ? 'Beneficiary entitlements' : 'Participant allocations'));
    for (const split of record.splits) {
      const input = record.splitMode === 'percentage' ? `${major(split.value, { minorUnits: 2 })}%` : record.splitMode === 'exact' ? money(split.value, record.currency) : split.value;
      details.append(node('p', `${recordUserLabel(split.user)} · input ${input} · ${money(split.amountMinor, record.currency)}${record.exchangeRate ? ` · ${money(split.settlementAmountMinor, record.settlementCurrency)} settlement` : ''}`));
    }
  }
  if (record.exchangeRate) details.append(node('p', `1 ${record.currency.code} = ${record.exchangeRate.rate} ${record.settlementCurrency.code}`), node('p', rateDescription(record.exchangeRate)));
  return details;
}
function receiptTotals(records) {
  const totals = new Map();
  for (const record of records) { const code = record.settlementCurrency.code; const row = totals.get(code) || { currency: record.settlementCurrency, amount: 0n }; row.amount += minor(record.settlementAmountMinor); totals.set(code, row); }
  return [...totals.values()].sort((a,b) => a.currency.code.localeCompare(b.currency.code)).map(row => money(row.amount, row.currency)).join(' · ') || 'No expenses';
}
function renderSettings(root, group) {
  root.append(pageHeader('Members & settings', 'Removing a member preserves their record history.', [button('Add member', () => openEditor('member'), 'primary', 'add-member')]));
  const list = node('div', null, 'entity-list');
  for (const member of group.members) {
    const owner = member.userId === group.owner.userId;
    list.append(entity([['Identity', userLabel(member)], ['Role', owner ? 'Owner' : 'Member']], owner ? [] : [button('Remove member', () => removeMember(group, member), 'danger', `remove-member-${member.userId}`)]));
  }
  root.append(list, append(node('section', null, 'card stack'), node('h2', 'Group settings'), node('p', groupPolicy(group)), group.currencyLocked && node('p', 'Currency settings are locked after the first record', 'muted'), append(node('div', null, 'row-actions'), button('Group settings', () => openEditor('rename'), 'secondary', 'group-settings'), button('Transfer ownership', () => openEditor('owner'), 'secondary', 'transfer-owner'), button('Delete group', () => deleteGroup(group), 'danger', 'delete-group'))));
}
function renderReport(root, group, report) {
  if (state.reportView?.groupId !== group.groupId || state.reportView?.epoch !== state.sessionGeneration) {
    state.reportView = { groupId: group.groupId, epoch: state.sessionGeneration, currency: null, sort: 'magnitude', direction: 'desc', expanded: new Set(), report };
  }
  const view = state.reportView; view.report = report;
  const sections = view.report.currencies;
  view.currency = sections.find(row => row.currency.code === view.currency)?.currency.code
    || sections.find(row => row.currency.code === group.defaultCurrency.code)?.currency.code
    || sections[0]?.currency.code || null;
  const content = node('div', null, 'stack');
  const recordError = node('div', null, 'error'); recordError.setAttribute('role', 'alert');
  const epoch = state.sessionGeneration; let recordRequest = 0;
  async function openRecord(recordId) {
    const request = ++recordRequest;
    const current = () => root.isConnected && recordError.isConnected && state.reportView === view && epoch === state.sessionGeneration && state.route.groupId === group.groupId && state.route.page === 'balances' && request === recordRequest;
    recordError.textContent = 'Loading record…';
    try {
      const record = await jsonFetch(`${groupApi(group.groupId)}/records/${encodeURIComponent(recordId)}`, {}, current);
      if (current()) { recordError.textContent = ''; openEditor(recordKind(record), record); }
    } catch (error) {
      if (current() && !error.stale) recordError.replaceChildren(node('span', errorMessage(error)), button('Retry opening record', () => openRecord(recordId)));
    }
  }
  const controls = node('div', null, 'toolbar');
  if (group.currencyMode === 'multi' || sections.length > 1) {
    const select = node('select'); select.dataset.focusKey = 'balance-currency';
    for (const section of sections) { const option = node('option', section.currency.code); option.value = section.currency.code; select.append(option); }
    select.value = view.currency || ''; select.disabled = !sections.length;
    select.onchange = () => { view.currency = select.value; render(); };
    controls.append(labeled('Settlement currency', select));
  }
  const sort = node('select'); sort.dataset.focusKey = 'balance-sort';
  for (const [value, text] of [['magnitude', 'Net magnitude'], ['net', 'Signed net balance'], ['name', 'Member name'], ['activity', 'Record activity']]) {
    const option = node('option', text); option.value = value; sort.append(option);
  }
  sort.value = view.sort; sort.onchange = () => { view.sort = sort.value; render(); };
  const direction = node('select'); direction.dataset.focusKey = 'balance-direction';
  for (const [value, text] of [['asc', 'Ascending'], ['desc', 'Descending']]) { const option = node('option', text); option.value = value; direction.append(option); }
  direction.value = view.direction; direction.onchange = () => { view.direction = direction.value; render(); };
  controls.append(labeled('Sort members', sort), labeled('Direction', direction));
  root.append(controls, recordError, content); render();
  function render() {
    const section = view.report.currencies.find(row => row.currency.code === view.currency);
    content.replaceChildren();
    if (!section) { content.append(node('p', 'No activity yet.', 'empty-state')); return; }
    const active = new Set(group.members.map(member => member.userId));
    const label = user => `${userLabel(user)}${active.has(user.userId) ? '' : ' · Former member'}`;
    const abs = value => value < 0n ? -value : value;
    const compare = (a, b) => a < b ? -1 : a > b ? 1 : 0;
    const fold = value => Array.from(value, character => character === 'ı' ? character : /\p{Script=Cherokee}/u.test(character) ? character.toUpperCase() : character.toUpperCase().toLowerCase()).join('').replace(/ß/g, 'ss');
    const rows = section.balances.map(balance => ({ balance, net: minor(balance.totalMinor), name: [fold(balance.user.displayName), fold(balance.user.username)] }));
    rows.sort((a, b) => {
      let primary;
      switch (view.sort) {
        case 'net': primary = compare(a.net, b.net); break;
        case 'name': primary = compare(a.name[0], b.name[0]) || compare(a.name[1], b.name[1]); break;
        case 'activity': primary = compare(a.balance.breakdown.length, b.balance.breakdown.length); break;
        default: primary = compare(abs(a.net), abs(b.net));
      }
      return (view.direction === 'desc' ? -primary : primary) || compare(a.balance.user.userId, b.balance.user.userId);
    });
    content.append(node('h2', `${section.currency.code} balances`));
    if (!rows.some(row => row.balance.breakdown.length)) content.append(node('p', 'No activity yet.', 'empty-state'));
    if (rows.length && rows.every(row => row.net === 0n)) content.append(node('p', 'All settled. Every member’s net balance is zero.', 'empty-state'));
    const metrics = [['Expenses paid', 'expensesPaidMinor'], ['Expense share', 'expenseShareMinor'], ['Income received', 'incomeReceivedMinor'], ['Income entitlement', 'incomeEntitlementMinor'], ['Payments sent', 'paymentsSentMinor'], ['Payments received', 'paymentsReceivedMinor']];
    for (const { balance, net } of rows) {
      const card = node('article', null, 'card stack balance-member');
      const status = net > 0n ? 'Gets back' : net < 0n ? 'Owes' : 'Settled';
      card.append(node('h3', label(balance.user)), node('p', `All-time net · ${status}: ${money(abs(net), section.currency)}`, 'balance-net'), node('p', `${balance.breakdown.length} distinct record${balance.breakdown.length === 1 ? '' : 's'}`, 'muted'));
      const grid = node('div', null, 'balance-metrics');
      for (const [title, key] of metrics) grid.append(field(title, money(balance[key], section.currency)));
      card.append(grid);
      const key = `${section.currency.code}:${balance.user.userId}`;
      const details = node('details'); details.open = view.expanded.has(key);
      details.append(node('summary', 'Per-record breakdown'));
      details.ontoggle = () => { if (details.isConnected) { if (details.open) view.expanded.add(key); else view.expanded.delete(key); } };
      const scroll = node('div', null, 'balance-table-scroll'); scroll.tabIndex = 0; scroll.setAttribute('role', 'region'); scroll.setAttribute('aria-label', `Per-record breakdown for ${userLabel(balance.user)}`);
      const table = node('table', null, 'balance-table');
      const caption = node('caption', `Saved settlement amounts and member balance impacts · ${section.currency.code}`);
      const head = node('tr');
      for (const title of ['Date', 'Type', 'Title', 'Record amount', 'Balance impact', 'Open record']) { const th = node('th', title); th.scope = 'col'; head.append(th); }
      table.append(caption, append(node('thead'), head));
      const body = node('tbody'); const stacked = node('div', null, 'balance-records');
      for (const row of balance.breakdown) {
        const delta = minor(row.amountMinor);
        const impact = node('span', `${delta > 0n ? '+' : delta < 0n ? '−' : ''}${money(abs(delta), section.currency)}`);
        impact.setAttribute('aria-label', `${delta > 0n ? 'Increases' : delta < 0n ? 'Decreases' : 'No change to'} balance${delta === 0n ? '' : ` by ${money(abs(delta), section.currency)}`}`);
        const type = ({ expense: 'Expense', income: 'Income', transfer: 'Payment / Transfer' })[row.recordType];
        const values = [row.date, type, row.title, money(row.recordAmountMinor, section.currency), impact];
        const tr = node('tr'); const mobile = node('article', null, 'balance-record stack');
        values.forEach((value, index) => {
          const td = node('td'); td.append(value instanceof Node ? value : document.createTextNode(value));
          if (index >= 3) td.className = 'balance-amount'; tr.append(td);
          mobile.append(field(['Date', 'Type', 'Title', 'Record amount', 'Balance impact'][index], value instanceof Node ? value.cloneNode(true) : value));
        });
        const action = layout => button(`Open record: ${row.title}`, () => openRecord(row.recordId), 'secondary', `balance-record-${layout}-${key}-${row.recordId}`);
        tr.append(append(node('td'), action('table'))); mobile.append(action('stack')); body.append(tr); stacked.append(mobile);
      }
      table.append(body); scroll.append(table); details.append(scroll, stacked);
      if (!balance.breakdown.length) details.append(node('p', 'No record activity.', 'muted'));
      card.append(details); content.append(card);
    }
    const payments = node('section', null, 'card stack'); payments.append(node('h2', 'Suggested payments'), node('p', 'Review payment details before saving.', 'muted'));
    for (const settlement of section.settlements) {
      const canPay = active.has(settlement.fromUser.userId) && active.has(settlement.toUser.userId);
      const action = button('Review payment', () => openEditor('transfer', null, { amount: major(settlement.amountMinor, section.currency), currency: section.currency, byUserId: settlement.fromUser.userId, toUserId: settlement.toUser.userId }));
      action.disabled = !canPay;
      const payment = append(node('div', null, 'stack balance-payment'), node('p', `${label(settlement.fromUser)} → ${label(settlement.toUser)} · ${money(settlement.amountMinor, section.currency)}`), action);
      if (!canPay) payment.append(node('p', 'Re-add former members before recording this payment.', 'muted'));
      payments.append(payment);
    }
    if (!section.settlements.length) payments.append(node('p', 'No suggested payments.', 'empty-state'));
    content.append(payments);
  }
}

function focusReference() {
  const element = document.activeElement;
  return { element, key: element?.dataset.focusKey };
}
function restoreFocus(reference) {
  if (reference?.element?.isConnected) reference.element.focus();
  else if (reference?.key) {
    const element = Array.from(document.querySelectorAll('[data-focus-key]')).find((item) => item.dataset.focusKey === reference.key);
    if (element) element.focus(); else $('#main-content').focus();
  }
}
function closeEditor(restore = true) {
  const editor = state.editor; state.editor = null;
  if (editor) for (const draft of editor.kind === 'receipt' ? editor.draft.records : [editor.draft]) retireDraft(draft);
  if ($('#editor-dialog').open) $('#editor-dialog').close();
  disposePickersWithin($('#editor-body')); $('#editor-body').replaceChildren(); $('#editor-error').textContent = '';
  if (restore && editor) restoreFocus(editor.focus);
  return editor?.focus;
}
function finishConfirmation(accepted) {
  const confirmation = state.confirmation; state.confirmation = null;
  if ($('#confirm-dialog').open) $('#confirm-dialog').close();
  $('#confirm-error').textContent = ''; $('#confirm-message').textContent = ''; $('#confirm-title').textContent = '';
  if (confirmation) { restoreFocus(confirmation.focus); confirmation.resolve(accepted); }
}
function confirmAction(title, message, action = null, acceptLabel = 'Confirm') {
  if (state.confirmation) return Promise.resolve(false);
  return new Promise((resolve) => {
    state.confirmation = { action, resolve, pending: false, focus: focusReference() };
    $('#confirm-title').textContent = title; $('#confirm-message').textContent = message; $('#confirm-error').textContent = '';
    $('#confirm-accept').textContent = acceptLabel; $('#confirm-accept').disabled = false; $('#confirm-cancel').disabled = false;
    $('#confirm-dialog').showModal(); $('#confirm-cancel').focus();
  });
}
async function acceptConfirmation() {
  const confirmation = state.confirmation;
  if (!confirmation || confirmation.pending) return;
  confirmation.pending = true; $('#confirm-accept').disabled = true; $('#confirm-cancel').disabled = true;
  $('#confirm-error').textContent = '';
  try {
    if (confirmation.action) await confirmation.action();
    if (state.confirmation === confirmation) finishConfirmation(true);
  } catch (error) {
    if (state.confirmation === confirmation) showError($('#confirm-error'), error);
  } finally {
    if (state.confirmation === confirmation) {
      confirmation.pending = false; $('#confirm-accept').disabled = false; $('#confirm-cancel').disabled = false;
    }
  }
}
async function requestDiscard(reloadStatistics = true) {
  if (!state.editor) return true;
  if (state.editor.pending) return false;
  if (state.editor.dirty && !await confirmAction('Discard unsaved changes?', 'Your unsaved changes will be lost.', null, 'Discard changes')) return false;
  const reference = closeEditor();
  if (reloadStatistics && state.route.page === 'statistics' && state.adminUsername) { await loadPage(); restoreFocus(reference); }
  return true;
}
async function destructive(title, impact, action, message, nextRoute = null) {
  const reference = focusReference();
  const accepted = await confirmAction(title, impact, action, 'Delete');
  if (!accepted || !state.adminUsername) return;
  announce(message);
  if (nextRoute) { location.hash = nextRoute; }
  else { await loadPage(); restoreFocus(reference); }
}
function deleteUser(user) {
  return destructive(`Delete user ${userLabel(user)}?`, 'This permanently deletes this account. Referenced group or record history may prevent deletion.', () => jsonFetch(`/admin/users/${encodeURIComponent(user.userId)}`, { method: 'DELETE' }), `Deleted user ${userLabel(user)}.`);
}
function deleteRecord(group, record) {
  const kind = recordKind(record);
  return destructive(`Delete ${kind} ${record.title || capitalize(kind)}?`, 'This permanently removes this record and its splits and changes the group balances.', () => jsonFetch(`${groupApi(group.groupId)}/records/${encodeURIComponent(record.recordId)}`, { method: 'DELETE' }), `${capitalize(kind)} deleted.`);
}
function deleteReceipt(group, receipt) {
  return destructive('Delete this receipt and its expenses?', `Receipt: ${receipt.note || '(No note)'}. This permanently deletes ${receipt.records.length} expenses and changes the group balances.`, () => jsonFetch(`${groupApi(group.groupId)}/receipts/${encodeURIComponent(receipt.receiptId)}`, { method: 'DELETE' }), 'Receipt and its expenses deleted.');
}
function deleteGroup(group) {
  return destructive(`Delete group ${group.groupName}?`, 'This permanently deletes the group, its records, receipts, and memberships.', () => jsonFetch(groupApi(group.groupId), { method: 'DELETE' }), 'Group deleted.', '#/groups');
}
async function removeMember(group, member) {
  const accepted = await confirmAction(`Remove member ${userLabel(member)}?`, `They will lose access to ${group.groupName}. Their record history and balances are preserved.`, () => jsonFetch(`${groupApi(group.groupId)}/members/${encodeURIComponent(member.userId)}`, { method: 'DELETE' }), 'Remove member');
  if (accepted && state.adminUsername) { announce('Member removed; history preserved.'); await loadPage(); }
}

const today = () => { const date = new Date(); return `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, '0')}-${String(date.getDate()).padStart(2, '0')}`; };
let currencyCatalog = null;
let currencyCatalogRequest = null;
async function getCurrencies(current) {
  if (currencyCatalog) return currencyCatalog;
  if (!currencyCatalogRequest) currencyCatalogRequest = jsonFetch('/currencies').then(rows => { currencyCatalog = rows; return rows; }).finally(() => { currencyCatalogRequest = null; });
  const rows = await currencyCatalogRequest; if (!current()) throw staleError(); return rows;
}
function needsConversion(draft) { return state.group.currencyMode === 'single' && draft.currency.code !== state.group.defaultCurrency.code; }
function invalidateRate(draft) { draft.rateGeneration = (draft.rateGeneration || 0) + 1; draft.rateLoading = false; }
function retireDraft(draft) { draft.active = false; invalidateRate(draft); draft.rateRender = null; draft.summaryRender = null; }
function rateInput(draft) {
  if (!needsConversion(draft)) return null;
  if (draft.rateLoading) throw new Error('Wait for the rate, or enter a manual rate.');
  if (draft.reference) return { source: 'reference', quoteId: draft.reference.quoteId };
  if (!draft.manualRate) throw new Error('Fetch a reference rate or enter a manual rate.');
  return { source: 'manual', rate: M.parseDecimalRate(draft.manualRate).text };
}
function rateValue(draft) { const input = rateInput(draft); return input ? M.parseDecimalRate(input.source === 'reference' ? draft.reference.rate : input.rate) : null; }
async function refreshRate(draft, dirty = true) {
  const editor = state.editor; if (!editor || editor.pending || !draft.active || !needsConversion(draft)) return;
  if (dirty) markDirty();
  invalidateRate(draft); const request = draft.rateGeneration; const epoch = state.sessionGeneration;
  const source = draft.currency.code, target = state.group.defaultCurrency.code;
  draft.reference = null; draft.manualRate = ''; draft.rateLoading = true; draft.rateError = ''; draft.rateRender?.();
  const current = () => state.editor === editor && epoch === state.sessionGeneration && draft.active && draft.rateGeneration === request && draft.currency.code === source && state.group.currencyMode === 'single' && state.group.defaultCurrency.code === target;
  try { const quote = await jsonFetch(`${groupApi(editor.groupId)}/exchange-quotes`, { method: 'POST', body: JSON.stringify({ currency: source }) }, current); if (current()) draft.reference = { ...quote, source: 'reference' }; }
  catch (error) { if (current() && !error.stale) { draft.rateError = errorMessage(error); if (error.status === 409) await refreshEditorPolicy(editor); } }
  finally { if (current()) { draft.rateLoading = false; draft.rateRender?.(); } }
}
function moneyDraft(record = null) {
  return { currency: record?.currency || state.group.defaultCurrency, active: true, rateGeneration: 0, rateLoading: false, rateError: '', manualRate: record?.exchangeRate?.source === 'manual' ? record.exchangeRate.rate : '', reference: record?.exchangeRate?.source === 'reference' ? { ...record.exchangeRate } : null };
}
function splitRecordDraft(record = null, kind = 'expense') {
  if (!['expense', 'income'].includes(kind)) throw new Error('Choose an expense or income kind.');
  if (record && recordKind(record) !== kind) throw new Error('Record kind does not match this editor.');
  const actor = record ? (kind === 'income' ? record.receivedBy : record.paidBy) : null;
  const draft = { ...moneyDraft(record), title: record?.title || '', note: record?.note ?? '', amount: record ? major(record.amountMinor, record.currency) : '', byUserId: actor?.userId || state.group.members[0]?.userId || '', date: record?.date || today(), mode: record?.splitMode || 'equal', selected: new Set(record ? record.splits.map(split => split.user.userId) : state.group.members.map(member => member.userId)), values: { shares: new Map(), percentage: new Map(), exact: new Map() }, visited: new Set(['equal']), historicalUsers: record ? [actor, ...record.splits.map(split => split.user)] : [] };
  if (record && draft.mode !== 'equal') { draft.visited.add(draft.mode); for (const split of record.splits) draft.values[draft.mode].set(split.user.userId, draft.mode === 'percentage' ? major(split.value, { minorUnits: 2 }) : draft.mode === 'exact' ? major(split.value, record.currency) : split.value); }
  return draft;
}
function draftAmount(draft, allowZero = false) { return M.parseMajorAmount(draft.amount, { minorDigits: draft.currency.minorUnits, allowZero }); }
function splitRows(draft) {
  return [...draft.selected].map(userId => {
    const raw = draft.values[draft.mode]?.get(userId) ?? '';
    const value = draft.mode === 'equal' ? 1n : draft.mode === 'percentage' ? M.parsePercentage(raw) : draft.mode === 'exact' ? M.parseMajorAmount(raw, { minorDigits: draft.currency.minorUnits, allowZero: true }) : /^\d+$/.test(raw.trim()) ? BigInt(raw.trim()) : (() => { throw new Error('Shares must be a whole number from 0 to 32767'); })();
    return { userId, value };
  });
}
function splitEqually(draft) {
  if (!draft.selected.size) return;
  let total = 10000n; if (draft.mode === 'exact') { try { total = draftAmount(draft); } catch (_) { total = 0n; } }
  for (const row of M.apportion(total, [...draft.selected].map(userId => ({ userId, value: 1n })))) draft.values[draft.mode].set(row.userId, major(row.value, { minorUnits: draft.mode === 'percentage' ? 2 : draft.currency.minorUnits }));
}
function enterMode(draft, mode) {
  draft.mode = mode;
  if (!draft.visited.has(mode)) {
    draft.visited.add(mode);
    if (mode === 'shares') for (const id of draft.selected) if (!draft.values.shares.has(id)) draft.values.shares.set(id, '1');
    if (mode === 'percentage' || mode === 'exact') splitEqually(draft);
  }
}
function markDirty() { if (state.editor) state.editor.dirty = true; }
function inputField(parent, title, draft, key, options = {}) {
  const input = node(options.multiline ? 'textarea' : 'input');
  if (!options.multiline) input.type = options.type || 'text';
  input.value = draft[key] ?? '';
  for (const name of ['required', 'min', 'max', 'step', 'minLength', 'maxLength', 'autocomplete']) if (options[name] !== undefined) input[name] = options[name];
  input.oninput = () => { draft[key] = input.value; markDirty(); if (options.changed) options.changed(); };
  parent.append(labeled(title, input)); return input;
}
function userLabel(user) { return `@${user.username} · ${user.displayName}`; }
function userChoice(user) { return { id: user.userId, primary: `@${user.username}`, secondary: user.displayName, entity: user }; }
function matchesUser(user, query) {
  const text = query.trim().toLocaleLowerCase();
  return text.startsWith('@') ? user.username.toLocaleLowerCase().includes(text.slice(1))
    : user.username.toLocaleLowerCase().includes(text) || user.displayName.toLocaleLowerCase().includes(text);
}
async function remoteUsers({ query, offset, limit, isCurrent = () => true }) {
  const page = await jsonFetch(buildPageUrl('/admin/users', { query, offset, limit, sort: 'username' }), {}, isCurrent);
  return { items: page.items.map((summary) => userChoice(summary.profile.user)), pageInfo: page.pageInfo };
}
const entityPickers = new Set();
let pickerSequence = 0;
function disposePickersWithin(root) {
  for (const picker of entityPickers) if (root === picker.element || root.contains(picker.element)) picker.dispose();
}
function entityPicker({ label, getSelected, loadPage, onSelect, disabledReason = () => null }) {
  const element = node('div', null, 'entity-picker');
  const trigger = button('', toggle, 'secondary picker-trigger');
  const panel = node('div', null, 'picker-panel'); panel.hidden = true;
  const panelId = `entity-picker-${++pickerSequence}`; panel.id = panelId;
  trigger.setAttribute('aria-controls', panelId); trigger.setAttribute('aria-expanded', 'false');
  const search = node('input'); search.type = 'search'; search.autocomplete = 'off';
  const searchLabel = labeled(`Search ${label.toLocaleLowerCase()}`, search);
  const results = node('ul', null, 'picker-results'); results.style.listStyle = 'none'; results.style.padding = '0'; results.style.margin = '0';
  const count = node('p', '', 'picker-count muted'); count.setAttribute('role', 'status'); count.setAttribute('aria-live', 'polite');
  const error = node('div', null, 'picker-error'); error.setAttribute('role', 'alert');
  const pagination = node('div', null, 'row-actions picker-pagination');
  append(panel, searchLabel, count, results, error, pagination); append(element, trigger, panel);
  const epoch = state.sessionGeneration;
  const editor = state.editor;
  let expanded = false, disposed = false, request = 0, timer = null, offset = 0;
  const limit = 20;
  const alive = () => !disposed && epoch === state.sessionGeneration && element.isConnected && (!editor || state.editor === editor);
  const pending = () => Boolean(editor && editor.pending);
  function disable(control, value) {
    if (pending()) { control.dataset.wasDisabled = String(value); control.disabled = true; }
    else { control.disabled = value; delete control.dataset.wasDisabled; }
  }
  function summary() {
    const choice = getSelected();
    trigger.textContent = `${label}: ${choice ? `${choice.primary}${choice.secondary ? ` · ${choice.secondary}` : ''}` : 'None selected'} — ${choice ? 'Change' : 'Choose'}`;
  }
  function invalidate() {
    clearTimeout(timer); timer = null; request++;
    results.querySelectorAll('button').forEach((control) => disable(control, true));
    pagination.querySelectorAll('button').forEach((control) => disable(control, true));
  }
  function collapse(restore = true) {
    invalidate(); expanded = false; panel.hidden = true; trigger.setAttribute('aria-expanded', 'false');
    if (restore && alive()) trigger.focus();
  }
  function toggle() {
    if (!alive() || pending()) return;
    if (expanded) { collapse(); return; }
    expanded = true; panel.hidden = false; trigger.setAttribute('aria-expanded', 'true');
    summary(); search.focus(); void fetchChoices();
  }
  async function fetchChoices() {
    invalidate();
    if (!expanded || !alive()) return;
    const generation = request;
    const current = () => alive() && expanded && generation === request;
    count.textContent = 'Loading…'; error.replaceChildren(); results.setAttribute('aria-busy', 'true');
    try {
      const page = await loadPage({ query: search.value, offset, limit, isCurrent: current });
      if (!current()) return;
      results.replaceChildren(); pagination.replaceChildren();
      const info = page.pageInfo; offset = info.offset;
      count.textContent = `${info.total} ${info.total === 1 ? 'result' : 'results'}${info.total ? ` · ${offset + 1}–${offset + page.items.length}` : ''}`;
      if (!page.items.length) results.append(node('li', search.value.trim() ? `No matching ${label.toLocaleLowerCase().includes('group') ? 'groups' : 'users'}` : label.toLocaleLowerCase().includes('group') ? 'No eligible groups' : 'No eligible members', 'empty-state'));
      for (const choice of page.items) {
        const reason = disabledReason?.(choice);
        const row = button('', () => {
          if (!current() || pending() || disabledReason?.(choice)) return;
          onSelect(choice); summary(); collapse();
        }, 'secondary picker-result');
        row.append(node('strong', choice.primary), node('span', choice.secondary, 'muted'));
        if (choice.id === getSelected()?.id) row.append(node('span', 'Selected', 'badge'));
        if (reason) row.append(node('span', reason, 'muted'));
        disable(row, Boolean(reason)); results.append(append(node('li'), row));
      }
      if (info.hasPrev || info.hasNext) {
        const previous = button('Previous', () => { if (!current() || pending()) return; offset = Math.max(0, offset - info.limit); void fetchChoices(); });
        const next = button('Next', () => { if (!current() || pending()) return; offset += info.limit; void fetchChoices(); });
        disable(previous, !info.hasPrev); disable(next, !info.hasNext); pagination.append(previous, next);
      }
    } catch (failure) {
      if (!current() || failure.stale) return;
      count.textContent = 'Could not load choices';
      const retry = button('Retry', () => { if (!pending()) void fetchChoices(); }); disable(retry, false);
      error.replaceChildren(node('p', errorMessage(failure), 'error'), retry);
    } finally { if (current()) results.removeAttribute('aria-busy'); }
  }
  search.oninput = () => {
    invalidate(); offset = 0; count.textContent = 'Searching…'; error.replaceChildren();
    timer = setTimeout(() => { timer = null; void fetchChoices(); }, 250);
  };
  search.onkeydown = (event) => {
    if (event.key === 'Enter') { event.preventDefault(); if (!pending()) void fetchChoices(); }
    else if (event.key === 'ArrowDown') { event.preventDefault(); results.querySelector('button:not(:disabled)')?.focus(); }
  };
  element.addEventListener('keydown', (event) => {
    if (event.key === 'Escape' && expanded) { event.preventDefault(); event.stopPropagation(); collapse(); }
  });
  const picker = {
    element,
    refresh() { summary(); if (expanded) void fetchChoices(); },
    dispose() { if (disposed) return; collapse(false); disposed = true; entityPickers.delete(picker); },
  };
  entityPickers.add(picker); summary(); return picker;
}
function memberPicker(parent, title, draft, key, membersOrProvider, onChange = null) {
  const members = () => typeof membersOrProvider === 'function' ? membersOrProvider() : membersOrProvider;
  const selected = () => {
    const id = draft[key]; if (!id) return null;
    const active = state.group?.members || members();
    const known = [...(draft.historicalUsers || []), ...(state.editor?.draft.historicalUsers || []), ...active].findLast((user) => user.userId === id);
    const choice = known ? userChoice(known) : { id, primary: 'Unavailable user', secondary: id.slice(0, 8), entity: null };
    if (!active.some((user) => user.userId === id)) choice.secondary += ' · Former member';
    return choice;
  };
  const picker = entityPicker({ label: title, getSelected: selected,
    loadPage: (args) => Promise.resolve(clientPage(members(), args)),
    onSelect: (choice) => { if (draft[key] !== choice.id) { draft[key] = choice.id; markDirty(); if (onChange) onChange(); } },
  });
  parent.append(picker.element); return picker;
}
function remoteUserPicker(parent, title, draft, key, disabledReason = null) {
  const known = new Map([...(draft.historicalUsers || []), ...(state.group?.members || [])].map((user) => [user.userId, user]));
  const picker = entityPicker({ label: title,
    getSelected: () => draft[key] ? known.has(draft[key]) ? userChoice(known.get(draft[key])) : { id: draft[key], primary: 'Unavailable user', secondary: draft[key].slice(0, 8), entity: null } : null,
    loadPage: remoteUsers, disabledReason,
    onSelect: (choice) => { known.set(choice.id, choice.entity); if (draft[key] !== choice.id) { draft[key] = choice.id; markDirty(); } },
  });
  parent.append(picker.element); return picker;
}
function currencyPicker(parent, label, draft, changed, locked = false) {
  const editor = state.editor; let catalog = null;
  const status = node('div', null, 'stack');
  const picker = entityPicker({ label, getSelected: () => ({ id: draft.currency.code, primary: draft.currency.code, secondary: draft.currency.name }),
    disabledReason: () => locked ? 'Currency settings are locked' : !catalog ? 'Load the currency catalogue first' : null,
    loadPage: async args => { const rows = await getCurrencies(() => state.editor === editor); catalog = rows; return clientPage(rows, args, c => ({ id: c.code, primary: c.code, secondary: c.name, entity: c })); },
    onSelect: choice => { if (draft.currency.code !== choice.id) { draft.currency = choice.entity; markDirty(); changed(); picker.refresh(); } },
  });
  parent.append(picker.element, status);
  async function load() {
    status.replaceChildren(node('p', 'Loading currency catalogue…', 'muted'));
    try { catalog = await getCurrencies(() => state.editor === editor && parent.isConnected); status.replaceChildren(); picker.refresh(); }
    catch (error) { if (!error.stale && state.editor === editor) status.replaceChildren(node('p', errorMessage(error), 'error'), button('Retry currencies', load)); }
  }
  // The form is attached before the asynchronous response is applied.
  queueMicrotask(load);
  return picker;
}
function renderCurrencyFields(parent, draft, changed = () => {}, { transfer = false, batch = false } = {}) {
  const review = node('p', '', 'muted'); const rates = node('div', null, 'stack');
  const currencyControl = currencyPicker(parent, batch ? 'Currency for every imported row' : 'Currency', draft, () => {
    invalidateRate(draft); draft.reference = null; draft.manualRate = ''; draft.rateError = '';
    review.textContent = `Review amounts in ${draft.currency.code}. Entered amounts are not converted.`;
    changed(); render(); if (!transfer && needsConversion(draft)) refreshRate(draft, false);
  }, transfer && state.group.currencyMode === 'single');
  parent.append(review, rates);
  if (batch) parent.append(node('p', 'Import each currency separately.', 'muted'));
  draft.rateRender = () => { if (rates.isConnected && draft.active && !state.editor?.pending) { render(); changed(); } };
  function render() {
    rates.replaceChildren();
    if (transfer) {
      if (state.group.currencyMode === 'single' && draft.currency.code !== state.group.defaultCurrency.code) rates.append(button(`Use settlement currency ${state.group.defaultCurrency.code}`, () => { draft.currency = state.group.defaultCurrency; markDirty(); review.textContent = `Review the unchanged amount in ${draft.currency.code}.`; currencyControl.refresh(); changed(); render(); }));
      return;
    }
    if (!needsConversion(draft)) return;
    const rate = node('input'); rate.type = 'text'; rate.inputMode = 'decimal'; rate.value = draft.reference?.rate || draft.manualRate; rate.disabled = Boolean(state.editor?.pending);
    rate.oninput = () => { invalidateRate(draft); draft.reference = null; draft.manualRate = rate.value; draft.rateError = ''; markDirty(); provenance.textContent = 'Manual'; rates.querySelector('.error')?.remove(); changed(); };
    rates.append(labeled(`Rate: 1 ${draft.currency.code} in ${state.group.defaultCurrency.code}`, rate));
    const provenance = node('p', draft.rateLoading ? 'Fetching daily reference…' : draft.reference ? rateDescription(draft.reference) : draft.manualRate ? 'Manual' : 'Choose a reference or manual rate.', 'muted'); rates.append(provenance);
    const refresh = button(draft.rateError ? 'Retry rate' : 'Refresh rate', () => refreshRate(draft)); refresh.disabled = Boolean(state.editor?.pending); rates.append(refresh);
    rates.append(button('Enter manually', () => { invalidateRate(draft); draft.reference = null; draft.manualRate = rate.value; draft.rateError = ''; markDirty(); render(); changed(); rates.querySelector('input')?.focus(); }));
    if (draft.rateError) rates.append(node('p', draft.rateError, 'error'));
  }
  render();
}
function renderSplitRecordFields(parent, draft, kind = 'expense') {
  if (!['expense', 'income'].includes(kind)) throw new Error('Choose an expense or income kind.');
  const grid = node('div', null, 'grid-2'); const preview = node('div', null, 'stack'); const splits = node('div', null, 'stack');
  inputField(grid, 'Title', draft, 'title', { required: true, changed: () => draft.summaryRender?.() });
  const amount = inputField(grid, 'Amount', draft, 'amount', { required: true, changed: updatePreview }); amount.inputMode = 'decimal';
  memberPicker(grid, kind === 'income' ? 'Received by' : 'Paid by', draft, 'byUserId', () => state.group.members, updatePreview);
  inputField(grid, 'Date', draft, 'date', { type: 'date', required: true }); parent.append(grid);
  inputField(parent, 'Note (optional)', draft, 'note', { multiline: true }).dataset.recordNote = '';
  let displayedCurrency = draft.currency.code;
  renderCurrencyFields(parent, draft, () => { if (displayedCurrency !== draft.currency.code) { displayedCurrency = draft.currency.code; renderSplits(); } updatePreview(); });
  const mode = node('select'); for (const [value, label] of [['equal','Equal'], ['shares','Shares'], ['percentage','Percentages'], ['exact','Exact amounts']]) { const option = node('option', label); option.value = value; mode.append(option); } mode.value = draft.mode;
  const search = node('input'); search.type = 'search'; search.placeholder = 'Handle or display name';
  const summary = node('p', '', 'muted'); const actions = node('div', null, 'row-actions');
  parent.append(labeled('Split method', mode), labeled(kind === 'income' ? 'Beneficiaries' : 'Search split participants', search), summary, actions, splits, preview);
  mode.onchange = () => { enterMode(draft, mode.value); markDirty(); renderSplits(); updatePreview(); };
  search.oninput = renderSplits;
  function renderSplits() {
    splits.replaceChildren(); actions.replaceChildren(); summary.textContent = `${draft.selected.size} ${kind === 'income' ? 'beneficiaries' : 'participants'} selected`;
    if (['percentage','exact'].includes(draft.mode)) actions.append(button('Split equally', () => { splitEqually(draft); markDirty(); renderSplits(); updatePreview(); }));
    const members = state.group.members; const known = new Map([...draft.historicalUsers, ...members].map(user => [user.userId, user]));
    const ids = [...new Set([...members.map(user => user.userId), ...draft.selected])];
    for (const id of ids) {
      const user = known.get(id), selected = draft.selected.has(id), former = !members.some(user => user.userId === id);
      if (!selected && (!user || !matchesUser(user, search.value))) continue;
      const label = user ? userLabel(user) : `Unavailable user ${id}`;
      const row = node('div', null, 'split-row'); const check = node('input'); check.type = 'checkbox'; check.checked = selected;
      check.onchange = () => { if (check.checked) { draft.selected.add(id); for (const m of ['shares','percentage','exact']) if (!draft.values[m].has(id)) draft.values[m].set(id, m === 'shares' ? '1' : '0'); } else draft.selected.delete(id); markDirty(); renderSplits(); updatePreview(); };
      row.append(labeled(`${label}${former ? ' — Former member' : ''}`, check));
      if (draft.mode !== 'equal') {
        const value = node('input'); value.type = 'text'; value.inputMode = draft.mode === 'shares' ? 'numeric' : 'decimal'; value.value = draft.values[draft.mode].get(id) ?? (draft.mode === 'shares' ? '1' : '0'); value.disabled = !selected;
        value.oninput = () => { draft.values[draft.mode].set(id, value.value); markDirty(); updatePreview(); };
        row.append(labeled(draft.mode === 'shares' ? 'Shares (0–32767)' : draft.mode === 'percentage' ? 'Percentage (%) — up to two decimals' : `Exact amount (${draft.currency.code})`, value));
        if (draft.mode === 'shares') for (const [label, delta] of [['−', -1n], ['+', 1n]]) { const control = button(label, () => { if (!/^\d+$/.test(value.value)) throw new Error('Enter whole shares first'); const next = BigInt(value.value) + delta; if (next < 0n || next > 32767n) return; value.value = next.toString(); value.oninput(); }); control.disabled = !selected; row.append(control); }
        else { const remainder = button('Assign remainder', () => { let total = draft.mode === 'percentage' ? 10000n : draftAmount(draft); for (const other of splitRows({ ...draft, selected: new Set([...draft.selected].filter(other => other !== id)) })) total -= other.value; if (total < 0n) throw new Error('Other allocations exceed the total'); draft.values[draft.mode].set(id, major(total, { minorUnits: draft.mode === 'percentage' ? 2 : draft.currency.minorUnits })); markDirty(); renderSplits(); updatePreview(); }); remainder.disabled = !selected; row.append(remainder); }
      }
      splits.append(row);
    }
  }
  function updatePreview() {
    preview.replaceChildren(node('h4', kind === 'income' ? 'Entitlement preview' : 'Split preview'));
    try {
      const amount = draftAmount(draft), rows = splitRows(draft);
      if (draft.mode === 'percentage' || draft.mode === 'exact') { const assigned = rows.reduce((sum,row) => sum + row.value, 0n), remaining = (draft.mode === 'percentage' ? 10000n : amount) - assigned; const fmt = value => draft.mode === 'percentage' ? `${major(value, { minorUnits: 2 })}%` : money(value, draft.currency); preview.append(node('p', `Assigned ${fmt(assigned)} · ${fmt(remaining < 0n ? -remaining : remaining)} ${remaining < 0n ? 'excess' : 'remaining'}`, remaining < 0n ? 'error' : 'muted')); }
      const known = new Map([...draft.historicalUsers, ...state.group.members].map(user => [user.userId, user]));
      const original = M.allocateSplits(draft.mode, amount, rows);
      let rate = null, settlement = amount, rateError = null;
      try { rate = rateValue(draft); if (rate) settlement = M.convertMinorAmount(amount, { sourceMinorDigits: draft.currency.minorUnits, targetMinorDigits: state.group.defaultCurrency.minorUnits, rate }); } catch (error) { rateError = error; }
      const converted = rate && !rateError ? M.apportion(settlement, rows) : null;
      if (converted) preview.append(node('p', `Settlement total ${money(settlement, state.group.defaultCurrency)}`));
      original.forEach((row, index) => preview.append(node('p', `${known.has(row.userId) ? userLabel(known.get(row.userId)) : row.userId}: ${money(row.value, draft.currency)}${converted ? ` · ${money(converted[index].value, state.group.defaultCurrency)} settlement` : ''}`)));
      if (rateError) preview.append(node('p', errorMessage(rateError), 'error'));
    } catch (error) { preview.append(node('p', errorMessage(error), 'error')); }
    if (!state.group.members.some(user => user.userId === draft.byUserId) || [...draft.selected].some(id => !state.group.members.some(user => user.userId === id))) preview.append(node('p', 'Re-add former members before saving. Their draft values are retained.', 'error'));
    draft.summaryRender?.();
  }
  renderSplits(); updatePreview();
}
function validateCommon(draft) {
  const amount = draftAmount(draft);
  if (!/^\d{4}-\d{2}-\d{2}$/.test(draft.date || '')) throw new Error('Choose a valid date.');
  if (!state.group.members.some(user => user.userId === draft.byUserId)) throw new Error('Re-add this member before saving changes. Choose an active member.');
  return amount;
}
function recordNoteInput(note) {
  if (note.includes('\0')) throw new Error('Note cannot contain NUL');
  return note.trim() ? note : null;
}
function validateSplitRecord(draft, kind = 'expense') {
  if (!['expense', 'income'].includes(kind)) throw new Error('Choose an expense or income kind.');
  const amount = validateCommon(draft); if (!draft.title.trim()) throw new Error('Title is required.');
  const rows = splitRows(draft); const error = M.validateMoneySplits(draft.mode, amount, rows); if (error) throw new Error(error);
  if (rows.some(row => !state.group.members.some(user => user.userId === row.userId))) throw new Error('Re-add this member before saving changes.');
  const exchangeRate = rateInput(draft); const rate = rateValue(draft); if (rate) M.convertMinorAmount(amount, { sourceMinorDigits: draft.currency.minorUnits, targetMinorDigits: state.group.defaultCurrency.minorUnits, rate });
  return { title: draft.title, note: recordNoteInput(draft.note), amountMinor: amount.toString(), currency: draft.currency.code, [kind === 'income' ? 'receivedByUserId' : 'byUserId']: draft.byUserId, date: draft.date, splitMode: draft.mode, splits: rows.map(row => ({ userId: row.userId, value: row.value.toString() })), exchangeRate };
}
function renderGroupCurrencySettings(parent, draft, locked) {
  const container = node('div', null, 'stack'); container.dataset.groupCurrencySettings = ''; parent.append(container);
  function render() {
    disposePickersWithin(container); container.replaceChildren();
    for (const [value, label] of [['single', 'Single currency settlement'], ['multi', 'Settle each currency separately']]) {
      const radio = node('input'); radio.type = 'radio'; radio.name = 'currency-mode'; radio.value = value; radio.checked = draft.currencyMode === value; radio.disabled = locked;
      radio.onchange = () => { if (radio.checked) { draft.currencyMode = value; markDirty(); render(); } }; container.append(labeled(label, radio));
    }
    currencyPicker(container, draft.currencyMode === 'single' ? 'Settlement currency' : 'Default entry currency', draft, () => {}, locked);
    if (locked) container.append(node('p', 'Currency settings are locked after the first record', 'muted'));
    if (state.editor?.pending) setEditorPending(true);
  }
  render();
}
async function refreshEditorPolicy(editor) {
  editor.policyReview = true;
  try {
    const previous = state.group; const group = await jsonFetch(groupApi(editor.groupId), {}, () => state.editor === editor);
    if (state.editor !== editor) return; state.group = group;
    editor.roleRender?.();
    const pairChanged = previous.currencyMode !== group.currencyMode || previous.defaultCurrency.code !== group.defaultCurrency.code;
    for (const draft of editor.kind === 'receipt' ? editor.draft.records : [editor.draft]) {
      if (pairChanged && draft.currency) { invalidateRate(draft); draft.reference = null; draft.manualRate = ''; draft.rateError = 'Group policy changed. Review this currency and choose a new rate if conversion is required.'; draft.rateRender?.(); }
    }
    editor.policyReview = true;
    const review = node('div', null, 'card stack'); review.append(node('p', `Current policy: ${groupPolicy(group)}. Review the retained draft before saving again.`, 'error'));
    review.append(button('I have reviewed the current policy', () => { editor.policyReview = false; review.remove(); })); $('#editor-body').prepend(review);
    if (editor.kind === 'rename') {
      editor.draft.currencyMode = group.currencyMode; editor.draft.currency = group.defaultCurrency;
      const old = $('#editor-body').querySelector('[data-group-currency-settings]'); if (old) { disposePickersWithin(old); old.remove(); }
      renderGroupCurrencySettings($('#editor-body'), editor.draft, group.currencyLocked);
    }
  } catch (error) { if (!error.stale && state.editor === editor) { $('#editor-error').textContent += ` Could not refresh group settings: ${errorMessage(error)}`; $('#editor-body').prepend(button('Retry group policy refresh', () => refreshEditorPolicy(editor))); } }
}
function transferRoleError(draft) {
  if (draft.byUserId && draft.toUserId && draft.byUserId === draft.toUserId) return 'Payer and recipient must be different';
  const members = state.group.members;
  if (members.length < 2) return 'At least two active members are required to save a payment. Add another member first.';
  if (!draft.byUserId || !draft.toUserId) return 'Choose an active payer and recipient.';
  if ([draft.byUserId, draft.toUserId].some(id => !members.some(member => member.userId === id))) return 'Re-add this member before saving changes.';
  return '';
}
function openEditor(kind, item = null, preset = null) {
  if (state.editor || !state.adminUsername) return;
  if (kind === 'authentication' && item?.environmentManaged) return;
  const group = state.group;
  if (item && ['expense', 'income', 'transfer'].includes(kind)) kind = recordKind(item);
  const draft = ['expense', 'income'].includes(kind) ? splitRecordDraft(item, kind)
    : kind === 'transfer' ? { ...moneyDraft(item), note: item?.note ?? '', amount: item ? major(item.amountMinor, item.currency) : '', byUserId: item?.paidBy.userId || group.members[0]?.userId || '', toUserId: item?.transferTo.userId || group.members[1]?.userId || '', date: item?.date || today(), historicalUsers: item ? [item.paidBy, item.transferTo] : [], ...preset }
    : kind === 'receipt' ? { note: item?.note ?? '', uploadedByUserId: item?.uploadedBy.userId || group.members[0]?.userId || '', records: item ? item.records.map(record => splitRecordDraft(record, 'expense')) : [] }
    : kind === 'rename' ? { name: group.groupName, currencyMode: group.currencyMode, currency: group.defaultCurrency }
    : kind === 'owner' || kind === 'member' ? { userId: '' }
    : kind === 'csv' ? { ...moneyDraft(), csv: '' }
    : kind === 'group' ? { name: '', userId: '', currencyMode: 'single', currency: currencyCatalog?.find(c => c.code === 'USD') || { code: 'USD', name: 'Default currency' } }
    : kind === 'authentication' ? { revision: item.revision, localRegistrationEnabled: item.localRegistrationEnabled, ssoRegistrationEnabled: item.ssoRegistrationEnabled, oidcEnabled: item.oidcEnabled, providerName: item.providerName, issuer: item.issuer ?? '', clientId: item.clientId ?? '', clientSecret: '' }
    : kind === 'user-profile' ? { username: item.profile.user.username, displayName: item.profile.user.displayName, email: item.profile.email, enabled: item.enabled }
    : { username: '', displayName: '', email: '', password: '', createMode: 'local' };
  state.editor = { kind, id: item?.recordId || item?.receiptId || (kind === 'user-profile' ? item.profile.user.userId : null), original: item, groupId: group?.groupId || null, draft, dirty: false, pending: false, error: '', focus: focusReference() };
  $('#editor-title').textContent = ({ authentication: 'Edit authentication', 'user-profile': 'Edit user profile', member: 'Add member', owner: 'Transfer ownership', rename: 'Group settings', csv: 'Import CSV' })[kind] || `${item ? 'Edit' : 'Create'} ${kind}`;
  $('#editor-error').textContent = ''; $('#editor-save').disabled = false; $('#editor-save').textContent = kind === 'csv' ? 'Import' : kind === 'rename' ? 'Save settings' : 'Save';
  $('#editor-cancel').disabled = false; $('#editor-close').disabled = false;
  const body = $('#editor-body'); body.replaceChildren();
  if (kind === 'authentication') renderAuthenticationEditor(body, draft, item);
  else if (kind === 'user-profile') {
    inputField(body, 'Username', draft, 'username', { required: true, maxLength: 50 });
    inputField(body, 'Display name', draft, 'displayName', { required: true, maxLength: 100 });
    inputField(body, 'Email', draft, 'email', { required: true, type: 'email', maxLength: 254 });
    checkboxField(body, 'Enabled', draft, 'enabled');
    body.append(node('p', 'Changing email marks it unverified. Disabling revokes all sessions and pending sign-in grants; re-enabling does not restore them.', 'muted'));
  }
  else if (['expense', 'income'].includes(kind)) renderSplitRecordFields(body, draft, kind);
  else if (kind === 'transfer') {
    const grid = node('div', null, 'grid-2'); body.append(grid);
    inputField(grid, 'Amount', draft, 'amount', { required: true }).inputMode = 'decimal';
    renderCurrencyFields(body, draft, () => {}, { transfer: true });
    inputField(grid, 'Date', draft, 'date', { type: 'date', required: true });
    inputField(body, 'Note (optional)', draft, 'note', { multiline: true }).dataset.recordNote = '';
    const roleError = node('p', '', 'error'); roleError.setAttribute('aria-live', 'polite'); body.append(roleError);
    const editor = state.editor;
    editor.roleRender = () => {
      const error = transferRoleError(draft);
      roleError.textContent = error; roleError.hidden = !error;
      $('#editor-save').disabled = editor.pending || Boolean(error);
      if (editor.error === 'Payer and recipient must be different' && draft.byUserId !== draft.toUserId) {
        editor.error = ''; $('#editor-error').textContent = '';
      }
    };
    memberPicker(grid, 'Paid by', draft, 'byUserId', () => state.group.members, editor.roleRender);
    memberPicker(grid, 'Paid to', draft, 'toUserId', () => state.group.members, editor.roleRender);
    editor.roleRender();
  } else if (kind === 'receipt') {
    if (item) body.append(node('p', 'Saving replaces all child expenses with new records. The uploader cannot be changed.', 'muted'), node('p', `Uploader: ${userLabel(item.uploadedBy)}`));
    inputField(body, 'Receipt note (optional)', draft, 'note', { multiline: true }).dataset.receiptNote = '';
    if (!item) memberPicker(body, 'Uploader', draft, 'uploadedByUserId', group.members);
    const items = node('div', null, 'stack'); body.append(items);
    const cards = new Map();
    function renderItems() {
      items.querySelector('.empty-state')?.remove();
      draft.records.forEach((record, index) => {
        let card = cards.get(record);
        if (!card) {
          const details = node('details', null, 'card stack'); details.open = true; const summary = node('summary'); details.append(summary);
          renderSplitRecordFields(details, record, 'expense');
          details.append(button('Remove expense', () => { if (state.editor?.pending || !draft.records.includes(record)) return; retireDraft(record); draft.records.splice(draft.records.indexOf(record), 1); disposePickersWithin(details); details.remove(); cards.delete(record); markDirty(); renderItems(); }, 'danger'));
          card = { details, summary }; cards.set(record, card); items.append(details);
        }
        record.summaryRender = () => {
          let description = `${record.currency.code} ${record.amount || '—'}`;
          try { const amount = draftAmount(record); description = money(amount, record.currency); const rate = rateValue(record); if (rate) description += ` · ${money(M.convertMinorAmount(amount, { sourceMinorDigits: record.currency.minorUnits, targetMinorDigits: state.group.defaultCurrency.minorUnits, rate }), state.group.defaultCurrency)} settlement`; } catch (_) { /* Invalid draft text stays visible for correction. */ }
          const payer = [...record.historicalUsers, ...state.group.members].findLast(user => user.userId === record.byUserId);
          card.summary.textContent = `Expense ${draft.records.indexOf(record) + 1} · ${record.title || 'Untitled'} · ${description} · ${payer ? userLabel(payer) : 'Choose payer'}`;
        }; record.summaryRender();
      });
      if (!draft.records.length) items.append(node('p', 'No expenses in this batch.', 'empty-state'));
    }
    renderItems();
    body.append(button('Add expense', () => { draft.records.push(splitRecordDraft(null, 'expense')); markDirty(); renderItems(); }));
  } else if (kind === 'csv') {
    body.append(node('p', 'Choose a UTF-8 CSV file or paste its contents below. Use decimal amounts. Quote comma-separated split lists; repeated names count as shares.', 'muted'), node('pre', 'title,amount,paidBy,split,date\nDinner,90,alice,"alice,bob,bob",2026-10-14', 'mono'));
    renderCurrencyFields(body, draft, () => {}, { batch: true });
    const file = node('input'); file.type = 'file'; file.accept = '.csv,text/csv';
    body.append(labeled('CSV file', file));
    const text = inputField(body, 'CSV content', draft, 'csv', { multiline: true, required: true }); text.rows = 12;
    file.onchange = guarded(async () => {
      const editor = state.editor; const selected = file.files?.[0]; if (!selected) return;
      const bytes = await selected.arrayBuffer();
      const content = new TextDecoder('utf-8', { fatal: true }).decode(bytes);
      if (state.editor !== editor) return;
      text.value = content; draft.csv = content; markDirty();
    }, $('#editor-error'));
  } else if (kind === 'owner') {
    memberPicker(body, 'New owner', draft, 'userId', group.members.filter((member) => member.userId !== group.owner.userId));
  } else if (kind === 'rename' || kind === 'group') {
    inputField(body, 'Group name', draft, 'name', { required: true, maxLength: 100 });
    renderGroupCurrencySettings(body, draft, kind === 'rename' && group.currencyLocked);
    if (kind === 'group') remoteUserPicker(body, 'Owner', draft, 'userId');
  } else if (kind === 'member') {
    remoteUserPicker(body, 'Member', draft, 'userId', (choice) => group.members.some((member) => member.userId === choice.id) ? 'Already a member' : null);
  } else {
    inputField(body, 'Username', draft, 'username', { required: true, maxLength: 50, autocomplete: 'off' });
    if (kind === 'user') { inputField(body, 'Display name', draft, 'displayName', { required: true, maxLength: 100 }); inputField(body, 'Email', draft, 'email', { required: true, type: 'email', maxLength: 254 }); }
    if (kind === 'user') {
      const mode = node('select');
      for (const [value, title] of [['local', 'Local password'], ['sso', 'SSO: pending sign-in setup']]) { const option = node('option', title); option.value = value; mode.append(option); }
      body.append(labeled('Create mode', mode));
      const password = inputField(body, 'Password', draft, 'password', { type: 'password', minLength: 6, required: true, autocomplete: 'new-password' });
      mode.onchange = () => { draft.createMode = mode.value; draft.password = ''; password.value = ''; password.required = mode.value === 'local'; password.disabled = mode.value === 'sso'; password.closest('label').hidden = mode.value === 'sso'; markDirty(); };
      body.append(node('p', 'SSO accounts require a ready provider and a linking invitation after creation.', 'muted'));
    } else inputField(body, 'Password', draft, 'password', { type: 'password', minLength: 6, required: true, autocomplete: 'new-password' });
  }
  $('#editor-dialog').showModal();
  const first = body.querySelector('input,select,textarea,button'); if (first) first.focus();
}
async function saveEditor(event) {
  event.preventDefault(); const editor = state.editor;
  if (!editor || editor.pending) return;
  const { kind, draft, id, groupId } = editor; const base = groupId ? groupApi(groupId) : '';
  let url, method = id ? 'PUT' : 'POST', payload, csv = false;
  $('#editor-error').textContent = ''; editor.error = '';
  if (editor.policyReview) { $('#editor-error').textContent = 'Review the current group policy before saving again.'; return; }
  try {
    if (['expense', 'income'].includes(kind)) { payload = validateSplitRecord(draft, kind); url = `${base}/records/${kind}${id ? `/${encodeURIComponent(id)}` : ''}`; }
    else if (kind === 'transfer') {
      const roleError = transferRoleError(draft); if (roleError) throw new Error(roleError);
      const amount = validateCommon(draft);
      if (!state.group.members.some((member) => member.userId === draft.toUserId)) throw new Error('Re-add this member before saving changes. Choose an active receiver.');
      if (state.group.currencyMode === 'single' && draft.currency.code !== state.group.defaultCurrency.code) throw new Error('Payments must use the group default currency');
      payload = { amountMinor: amount.toString(), currency: draft.currency.code, byUserId: draft.byUserId, toUserId: draft.toUserId, date: draft.date, note: recordNoteInput(draft.note) }; url = `${base}/records/transfer${id ? `/${encodeURIComponent(id)}` : ''}`;
    } else if (kind === 'receipt') {
      payload = { note: draft.note, records: draft.records.map(record => validateSplitRecord(record, 'expense')) };
      if (!id) {
        if (!state.group.members.some((member) => member.userId === draft.uploadedByUserId)) throw new Error('Choose an active member as uploader.');
        payload.uploadedByUserId = draft.uploadedByUserId;
      }
      url = `${base}/receipts${id ? `/${encodeURIComponent(id)}` : ''}`;
    } else if (kind === 'csv') {
      if (!draft.csv.trim()) throw new Error('Choose or paste CSV content first.');
      const params = new URLSearchParams({ currency: draft.currency.code }); const rate = rateInput(draft);
      if (rate?.source === 'reference') params.set('exchangeQuoteId', rate.quoteId); else if (rate) params.set('manualRate', rate.rate);
      csv = true; payload = draft.csv; url = `${base}/import?${params}`;
    } else if (kind === 'rename') { url = base; method = 'PUT'; payload = { groupName: draft.name, currencyMode: draft.currencyMode, defaultCurrency: draft.currency.code }; }
    else if (kind === 'member') { url = `${base}/members`; payload = draft.userId; }
    else if (kind === 'owner') {
      const owner = state.group.members.find((member) => member.userId === draft.userId && member.userId !== state.group.owner.userId);
      if (!owner) throw new Error('Choose another active member as the new owner.');
      const confirmed = await confirmAction(`Transfer ownership to ${userLabel(owner)}?`, `They will become the owner of ${state.group.groupName}.`, null, 'Transfer ownership');
      if (!confirmed || state.editor !== editor) return;
      url = `${base}/owner`; method = 'PUT'; payload = draft.userId;
    } else if (kind === 'group') { url = '/admin/groups'; payload = { createGroupName: draft.name, ownerUserId: draft.userId, currencyMode: draft.currencyMode, defaultCurrency: draft.currency.code }; }
    else if (kind === 'authentication') {
      payload = authenticationPayload(draft); url = '/admin/authentication'; method = 'PUT';
      if (editor.original.oidcEnabled && !draft.oidcEnabled && !await confirmAction('Disable SSO?', `${editor.original.ssoOnlyUserCount} enabled users have only SSO. SSO sessions and pending SSO flows/invitations will be revoked. Password sign-in remains available.`, null, 'Disable SSO')) return;
      if (state.editor !== editor) return;
    }
    else if (kind === 'user-profile') {
      url = userApi(id); method = 'PUT'; payload = { ...draft };
      if (editor.original.enabled && !draft.enabled && !await confirmAction('Disable user?', 'All sessions and outstanding sign-in grants will be revoked. Ledger history is preserved. Re-enabling will not restore those sessions.', null, 'Disable user')) return;
      if (state.editor !== editor) return;
    }
    else if (kind === 'user') { url = '/admin/users'; payload = { createUsername: draft.username, createDisplayName: draft.displayName, createEmail: draft.email, createMode: draft.createMode, createPassword: draft.createMode === 'local' ? draft.password : null }; }
    else if (kind === 'admin') { url = '/admin/admins'; payload = { createAdminUsername: draft.username, createAdminPassword: draft.password }; }
    if ((kind === 'group' || kind === 'rename') && !draft.name.trim()) throw new Error('Group name is required.');
    if (['member', 'group'].includes(kind) && !draft.userId) throw new Error('Choose a user.');
    if (['user', 'admin'].includes(kind) && !draft.username) throw new Error('Username is required.');
    if ((kind === 'admin' || (kind === 'user' && draft.createMode === 'local')) && draft.password.length < 6) throw new Error('Password must have at least 6 characters.');
    if ((kind === 'admin' || (kind === 'user' && draft.createMode === 'local')) && new TextEncoder().encode(draft.password).length > 72) throw new Error('Password must be at most 72 UTF-8 bytes');
    editor.pending = true; setEditorPending(true);
    const response = await jsonFetch(url, { method, body: csv ? payload : JSON.stringify(payload), ...(csv ? { headers: { 'Content-Type': 'text/csv' } } : {}) }, () => state.editor === editor);
    if (state.editor !== editor) return;
    const reference = closeEditor(false);
    announce(csv ? response.length ? `Imported ${response.length} expenses.` : 'No expense rows found.' : 'Changes saved.');
    if (kind === 'user') {
      announce(draft.createMode === 'sso' ? 'User created. Generate an SSO linking invitation to complete sign-in setup.' : 'User created.');
      location.hash = userPath(response.profile.user.userId);
      return;
    }
    // Lists are reloaded from the server; there is no stale record/receipt/report cache.
    await loadPage(); restoreFocus(reference);
  } catch (error) {
    if (state.editor === editor && !error.stale) { editor.error = errorMessage(error); $('#editor-error').textContent = editor.error; }
    if (error.status === 409 && state.editor === editor && editor.groupId && !['authentication', 'user-profile'].includes(kind)) await refreshEditorPolicy(editor);
  } finally { if (state.editor === editor) { editor.pending = false; setEditorPending(false); } }
}
function setEditorPending(pending) {
  $('#editor-save').disabled = pending; $('#editor-cancel').disabled = pending; $('#editor-close').disabled = pending;
  $('#editor-save').textContent = pending ? 'Saving…' : state.editor?.kind === 'csv' ? 'Import' : state.editor?.kind === 'rename' ? 'Save settings' : 'Save';
  $('#editor-body').querySelectorAll('button,input,select,textarea').forEach((control) => {
    if (pending) { if (control.dataset.wasDisabled === undefined) control.dataset.wasDisabled = String(control.disabled); control.disabled = true; }
    else if (control.dataset.wasDisabled !== undefined) { control.disabled = control.dataset.wasDisabled === 'true'; delete control.dataset.wasDisabled; }
  });
  if (!pending && state.editor) for (const draft of state.editor.kind === 'receipt' ? state.editor.draft.records : [state.editor.draft]) draft.rateRender?.();
  state.editor?.roleRender?.();
}
async function login(event) {
  event.preventDefault(); if (state.loginPending) return;
  const form = $('#login-form'); const data = new FormData(form);
  const epoch = ++state.sessionGeneration;
  state.generation++;
  state.loginPending = true; const submit = form.querySelector('button'); submit.disabled = true;
  $('#login-error').textContent = ''; $('#session-retry').classList.add('hidden');
  try {
    const session = await jsonFetch('/admin/auth/session/login', { method: 'POST', body: JSON.stringify({ username: data.get('username'), password: data.get('password') }) });
    if (epoch !== state.sessionGeneration) return;
    state.adminUsername = session.summaryUsername; form.reset(); renderShell(); await routeChanged();
  } catch (error) { showError($('#login-error'), error); }
  finally { state.loginPending = false; submit.disabled = false; }
}
async function logout() {
  if (!await requestDiscard(false)) return;
  const control = $('#logout-btn'); if (control.disabled) return; control.disabled = true;
  clearSession();
  try {
    await jsonFetch('/admin/auth/session/logout', { method: 'POST' });
  } catch (error) {
    if (!error.stale) {
      $('#login-error').textContent = 'Local session cleared, but the server could not confirm sign-out. Retry sign-out before using a shared device.';
      $('#logout-retry').classList.remove('hidden');
    }
  }
  finally { control.disabled = false; }
}
async function bootstrap() {
  $('#session-retry').classList.add('hidden'); $('#login-error').textContent = '';
  const epoch = state.sessionGeneration;
  try {
    const session = await jsonFetch('/admin/auth/session/me');
    if (epoch !== state.sessionGeneration) return;
    state.adminUsername = session.summaryUsername; renderShell(); await routeChanged();
  } catch (error) {
    if (!error.stale) { $('#login-error').textContent = 'Could not check your session. Retry, or sign in below.'; $('#session-retry').classList.remove('hidden'); }
  }
}
$('#login-form').onsubmit = guarded(login, $('#login-error'));
$('#logout-btn').onclick = guarded(logout);
$('#logout-retry').onclick = guarded(logout, $('#login-error'));
$('#session-retry').onclick = guarded(bootstrap, $('#login-error'));
$('#editor-form').onsubmit = guarded(saveEditor, $('#editor-error'));
// Capture non-bubbling invalid events before native validation focuses a field.
$('#editor-form').addEventListener('invalid', (event) => {
  for (let ancestor = event.target.parentElement; ancestor && ancestor !== event.currentTarget; ancestor = ancestor.parentElement) {
    if (ancestor.tagName === 'DETAILS') ancestor.open = true;
  }
}, true);
$('#editor-close').onclick = guarded(requestDiscard, $('#editor-error'));
$('#editor-cancel').onclick = guarded(requestDiscard, $('#editor-error'));
$('#editor-dialog').addEventListener('cancel', guarded(async (event) => { event.preventDefault(); await requestDiscard(); }, $('#editor-error')));
$('#confirm-accept').onclick = guarded(acceptConfirmation, $('#confirm-error'));
$('#confirm-cancel').onclick = () => { if (!state.confirmation?.pending) finishConfirmation(false); };
$('#confirm-dialog').addEventListener('cancel', (event) => { event.preventDefault(); if (!state.confirmation?.pending) finishConfirmation(false); });
window.addEventListener('hashchange', guarded(routeChanged));
window.addEventListener('beforeunload', (event) => { if (state.editor?.dirty) { event.preventDefault(); event.returnValue = ''; } });
renderShell();
guarded(bootstrap, $('#login-error'))();
