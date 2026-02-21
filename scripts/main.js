/**
 * Overlap Token Viewer – Main entry point
 *
 * Automatically displays overlapping tokens on the same grid cell as pie-chart
 * slices, with hover highlighting, click-to-select, right-click targeting, and
 * a keyboard shortcut to open a token list popup.
 *
 * Designed for Foundry VTT v13.
 */

const MODULE_ID = "overlap-token-viewer";

/* ──────────────────────────────────────────────────────────────────────────────
   Utility helpers
   ────────────────────────────────────────────────────────────────────────────── */

/**
 * Obtain a grid-cell key string for a token's primary (top-left) position.
 * @param {Token} token
 * @returns {string} "i,j"
 */
function getTokenCellKey(token) {
  const grid = canvas.grid;
  if (!grid || grid.isGridless) return `${token.document.x},${token.document.y}`;
  const offset = grid.getOffset({ x: token.document.x, y: token.document.y });
  return `${offset.i},${offset.j}`;
}

/**
 * Return the size category for a token: width * height.
 * @param {Token} token
 * @returns {number}
 */
function getTokenSize(token) {
  return token.document.width * token.document.height;
}

/**
 * Return all grid cell keys that a token occupies.
 * @param {Token} token
 * @returns {string[]}
 */
function getTokenCellKeys(token) {
  const grid = canvas.grid;
  if (!grid || grid.isGridless) return [`${token.document.x},${token.document.y}`];
  const topLeft = grid.getOffset({ x: token.document.x, y: token.document.y });
  const w = token.document.width;
  const h = token.document.height;
  const keys = [];
  for (let di = 0; di < h; di++) {
    for (let dj = 0; dj < w; dj++) {
      keys.push(`${topLeft.i + di},${topLeft.j + dj}`);
    }
  }
  return keys;
}

/**
 * Build the overlap data structures.
 *
 * Returns an object with:
 *  - cellMap: Map<cellKey, Token[]>  (all tokens on each cell, for hover/click)
 *  - pieGroups: Array<{tokens: Token[], anchorCell: string}>
 *      Groups of same-size tokens that share the same position and need
 *      pie-slicing among themselves.
 *  - bigTokenMasks: Map<tokenId, {token, holeCells: string[]}>
 *      Large tokens that have smaller tokens on top. For each, we record
 *      which of their cells are covered by smaller tokens.
 */
function buildOverlapData(minCount = 2) {
  const grid = canvas.grid;

  // Step 1: Build a map of cellKey → all tokens covering that cell
  const cellMap = new Map();
  for (const token of canvas.tokens.placeables) {
    if (!token.visible) continue;
    const keys = getTokenCellKeys(token);
    for (const key of keys) {
      if (!cellMap.has(key)) cellMap.set(key, []);
      cellMap.get(key).push(token);
    }
  }

  // Remove cells with fewer than minCount tokens
  for (const [key, arr] of cellMap) {
    if (arr.length < minCount) cellMap.delete(key);
  }

  // Step 2: Find pie-slice groups.
  // Tokens of the same size sharing the same top-left cell key are grouped.
  // We use a compound key of "size|topLeftCellKey" to avoid duplicates.
  const pieGroupMap = new Map();  // "size|cellKey" → Set<tokenId>
  const tokenById = new Map();    // tokenId → token

  for (const [, tokens] of cellMap) {
    for (const token of tokens) {
      tokenById.set(token.id, token);
    }
  }

  // For each cell, find same-size tokens that share the same top-left cell
  for (const [, tokens] of cellMap) {
    // Group by size
    const bySize = new Map();
    for (const token of tokens) {
      const size = getTokenSize(token);
      if (!bySize.has(size)) bySize.set(size, []);
      bySize.get(size).push(token);
    }

    for (const [size, sameSize] of bySize) {
      if (sameSize.length < 2) continue;
      // Further group by top-left cell key (same-size tokens must share
      // the same origin to be pie-sliced together)
      const byOrigin = new Map();
      for (const t of sameSize) {
        const originKey = getTokenCellKey(t);
        if (!byOrigin.has(originKey)) byOrigin.set(originKey, []);
        byOrigin.get(originKey).push(t);
      }
      for (const [originKey, group] of byOrigin) {
        if (group.length < 2) continue;
        const compoundKey = `${size}|${originKey}`;
        if (!pieGroupMap.has(compoundKey)) {
          pieGroupMap.set(compoundKey, new Set());
        }
        for (const t of group) {
          pieGroupMap.get(compoundKey).add(t.id);
        }
      }
    }
  }

  const pieGroups = [];
  for (const [compoundKey, idSet] of pieGroupMap) {
    const tokens = [...idSet].map(id => tokenById.get(id)).filter(Boolean);
    if (tokens.length < 2) continue;
    const anchorCell = compoundKey.split("|").slice(1).join("|");
    pieGroups.push({ tokens, anchorCell });
  }

  // Step 3: Find big tokens that need hole-masking.
  // A big token needs a hole on a cell if that cell has a smaller token on it.
  const bigTokenMasks = new Map();  // tokenId → { token, holeCells: Set<cellKey> }

  for (const [cellKey, tokens] of cellMap) {
    // Find the smallest token size on this cell
    let minSize = Infinity;
    for (const t of tokens) {
      const s = getTokenSize(t);
      if (s < minSize) minSize = s;
    }
    // Any token bigger than minSize needs a hole on this cell
    for (const t of tokens) {
      if (getTokenSize(t) <= minSize) continue;
      if (!bigTokenMasks.has(t.id)) {
        bigTokenMasks.set(t.id, { token: t, holeCells: new Set() });
      }
      bigTokenMasks.get(t.id).holeCells.add(cellKey);
    }
  }

  return { cellMap, pieGroups, bigTokenMasks };
}

/* ──────────────────────────────────────────────────────────────────────────────
   PIXI Graphics: pie-slice masks
   ────────────────────────────────────────────────────────────────────────────── */

/**
 * Calculate the slice geometry (start angle, end angle) for N items.
 * Slices are arranged like a pie chart starting from the top (−π/2).
 */
function getSliceAngles(index, total) {
  const sliceAngle = (Math.PI * 2) / total;
  const startAngle = -Math.PI / 2 + sliceAngle * index;
  const endAngle = startAngle + sliceAngle;
  return { startAngle, endAngle };
}

/**
 * Draw a pie-slice shape on a PIXI.Graphics.
 * @param {PIXI.Graphics} g
 * @param {number} cx  centre x
 * @param {number} cy  centre y
 * @param {number} r   radius
 * @param {number} startAngle
 * @param {number} endAngle
 */
function drawSlice(g, cx, cy, r, startAngle, endAngle) {
  g.moveTo(cx, cy);
  g.lineTo(cx + Math.cos(startAngle) * r, cy + Math.sin(startAngle) * r);
  const steps = 40;
  const delta = (endAngle - startAngle) / steps;
  for (let i = 1; i <= steps; i++) {
    const a = startAngle + delta * i;
    g.lineTo(cx + Math.cos(a) * r, cy + Math.sin(a) * r);
  }
  g.closePath();
}

/**
 * Determine which slice index an angle falls within.
 * Uses the midpoint-distance method for robustness across the ±π boundary.
 * @param {number} angle  The mouse angle (from atan2)
 * @param {number} total  Number of slices
 * @returns {number} slice index (0-based)
 */
function getSliceIndexForAngle(angle, total) {
  let bestIndex = 0;
  let bestDist = Infinity;
  for (let i = 0; i < total; i++) {
    const { startAngle, endAngle } = getSliceAngles(i, total);
    const mid = (startAngle + endAngle) / 2;
    let d = Math.abs(angle - mid);
    // Handle wrap-around
    if (d > Math.PI) d = Math.PI * 2 - d;
    if (d < bestDist) {
      bestDist = d;
      bestIndex = i;
    }
  }
  return bestIndex;
}

/* ──────────────────────────────────────────────────────────────────────────────
   Core overlay manager
   ────────────────────────────────────────────────────────────────────────────── */

class OverlapTokenManager {
  constructor() {
    /** @type {Map<string, Token[]>} cell-key → tokens covering that cell */
    this.overlapMap = new Map();
    /** @type {Array<{tokens: Token[], anchorCell: string}>} pie-slice groups */
    this.pieGroups = [];
    /** @type {Map<string, {token: Token, holeCells: Set<string>}>} */
    this.bigTokenMasks = new Map();
    /** @type {Set<string>} token ids that are part of any overlap group */
    this.managedTokenIds = new Set();
    /**
     * Per-token-id mask data.
     * Stores both the mesh mask and the token-container mask, plus originals.
     * @type {Map<string, {meshMaskGfx: PIXI.Graphics, meshOriginalMask: any, tokenMaskGfx: PIXI.Graphics, tokenOriginalMask: any, tokenOriginalHitArea: any}>}
     */
    this.maskData = new Map();
    /** @type {Map<string, PIXI.Graphics>} per-token-id highlight graphics */
    this.highlights = new Map();
    /** @type {string|null} token id currently highlighted by hover */
    this.hoveredTokenId = null;
    /** @type {PIXI.Container|null} overlay container for highlights */
    this.overlayContainer = null;
    /** @type {boolean} is the popup list open? */
    this.popupOpen = false;
    /** Debounce timer for refresh */
    this._refreshTimer = null;
    /** Track whether the module is active */
    this.active = false;
    /** Bound handler for closing the popup on outside click */
    this._popupCloseHandler = null;
  }

  /* ── lifecycle ────────────────────────────────────────── */

  init() {
    this.active = true;
    this._createContainer();
    this.refresh();
  }

  tearDown() {
    this.active = false;
    this._clearAllVisuals();
    if (this.overlayContainer) {
      this.overlayContainer.destroy({ children: true });
      this.overlayContainer = null;
    }
    this.closePopup();
  }

  _createContainer() {
    if (this.overlayContainer && !this.overlayContainer.destroyed) return;
    this.overlayContainer = new PIXI.Container();
    this.overlayContainer.name = "otv-overlay";
    this.overlayContainer.eventMode = "none";
    // Add to the interface layer so it renders above tokens
    canvas.tokens.addChild(this.overlayContainer);
  }

  /* ── main refresh ────────────────────────────────────── */

  scheduleRefresh() {
    if (this._refreshTimer) clearTimeout(this._refreshTimer);
    this._refreshTimer = setTimeout(() => this.refresh(), 80);
  }

  refresh() {
    if (!this.active || !canvas?.ready) return;
    this._clearAllVisuals();
    const minCount = game.settings?.get(MODULE_ID, "minOverlap") ?? 2;
    const data = buildOverlapData(minCount);
    this.overlapMap = data.cellMap;
    this.pieGroups = data.pieGroups;
    this.bigTokenMasks = data.bigTokenMasks;

    // Collect all managed token ids
    this.managedTokenIds.clear();
    for (const [, tokens] of this.overlapMap) {
      for (const t of tokens) this.managedTokenIds.add(t.id);
    }

    if (this.overlapMap.size === 0) return;
    this._createContainer();

    // Apply pie-slice masks for same-size overlapping token groups
    for (const group of this.pieGroups) {
      this._applyPieSlices(group.tokens);
    }

    // Apply hole-masks for big tokens that have smaller tokens on top
    for (const [, info] of this.bigTokenMasks) {
      this._applyBigTokenHoleMask(info.token, info.holeCells);
    }

    // Create highlights for lone small tokens (managed but not masked).
    // These are tokens on a big token's cell that have no same-size peer
    // (so no pie-slicing) and are not big tokens themselves.
    for (const tokenId of this.managedTokenIds) {
      if (this.highlights.has(tokenId)) continue; // already has a highlight
      const token = canvas.tokens?.get(tokenId);
      if (!token) continue;
      this._createSimpleHighlight(token);
    }
  }

  /* ── visual creation ─────────────────────────────────── */

  /**
   * Apply pie-slice masks to a group of same-size tokens that share the
   * same top-left cell. Works for any token size (1×1, 2×2, 3×3, etc.).
   *
   * If the token already has a hole-mask from _applyBigTokenHoleMask, the
   * pie-slice is combined with it (but big tokens with holes are handled
   * separately, so in practice pie-sliced tokens don't also have holes).
   */
  _applyPieSlices(tokens) {
    const total = tokens.length;
    // Sort tokens for a deterministic order (by id)
    tokens.sort((a, b) => a.id.localeCompare(b.id));

    for (let i = 0; i < total; i++) {
      const token = tokens[i];
      const { startAngle, endAngle } = getSliceAngles(i, total);

      // ──────────────────────────────────────────────────────────
      //  FVTT v13 dual rendering architecture:
      //
      //  1. Token container (PlaceableObject/PIXI.Container)
      //     Local (0,0) is top-left. We mask to clip border & bars.
      //
      //  2. token.mesh (PrimarySpriteMesh extends SpriteMesh extends Container)
      //     anchor (0.5, 0.5) so local (0,0) is centre.
      //     mesh.width/height are SCALED; use TEXTURE dims for child coords.
      // ──────────────────────────────────────────────────────────

      // === MASK 1: Token container (for border, bars, effects) ===
      const tokenW = token.w;  // pixel width on screen (may be >1 cell)
      const tokenH = token.h;
      const tokenCx = tokenW / 2;
      const tokenCy = tokenH / 2;
      const tokenR = Math.max(tokenW, tokenH) * 0.75;

      const tokenMaskGfx = new PIXI.Graphics();
      tokenMaskGfx.beginFill(0xffffff);
      drawSlice(tokenMaskGfx, tokenCx, tokenCy, tokenR, startAngle, endAngle);
      tokenMaskGfx.endFill();

      const tokenOriginalMask = token.mask;
      const tokenOriginalHitArea = token.hitArea;
      token.addChild(tokenMaskGfx);
      token.mask = tokenMaskGfx;

      // Override hitArea so PIXI hit-testing covers the FULL token bounds
      token.hitArea = new PIXI.Rectangle(0, 0, tokenW, tokenH);

      // === MASK 2: Mesh (for the token image) ===
      const mesh = token.mesh;
      let meshMaskGfx = null;
      let meshOriginalMask = null;

      if (mesh) {
        const tex = mesh.texture;
        const texW = tex?.width ?? 1;
        const texH = tex?.height ?? 1;
        const meshCx = 0;
        const meshCy = 0;
        const meshR = Math.max(texW, texH) * 0.75;

        meshMaskGfx = new PIXI.Graphics();
        meshMaskGfx.beginFill(0xffffff);
        drawSlice(meshMaskGfx, meshCx, meshCy, meshR, startAngle, endAngle);
        meshMaskGfx.endFill();

        meshOriginalMask = mesh.mask;
        mesh.addChild(meshMaskGfx);
        mesh.mask = meshMaskGfx;
      }

      this.maskData.set(token.id, {
        meshMaskGfx,
        meshOriginalMask,
        tokenMaskGfx,
        tokenOriginalMask,
        tokenOriginalHitArea,
      });

      // --- Highlight overlay (drawn in our overlay container, in scene coords) ---
      const hlGfx = new PIXI.Graphics();
      hlGfx.eventMode = "none";
      hlGfx.visible = false;

      const tx = token.document.x;
      const ty = token.document.y;
      const hlCx = tx + tokenW / 2;
      const hlCy = ty + tokenH / 2;
      const hlR = tokenR;

      hlGfx.beginFill(0xffff00, 0.22);
      drawSlice(hlGfx, hlCx, hlCy, hlR, startAngle, endAngle);
      hlGfx.endFill();
      hlGfx.lineStyle(2, 0xffff00, 0.6);
      drawSlice(hlGfx, hlCx, hlCy, hlR, startAngle, endAngle);

      this.overlayContainer.addChild(hlGfx);
      this.highlights.set(token.id, hlGfx);
    }
  }

  /**
   * Apply a hole-mask to a big token. The big token is fully rendered EXCEPT
   * on the cells where smaller tokens sit — those cells are punched out so
   * the smaller tokens underneath (or on top in z-order) show through.
   *
   * The mask is a filled rectangle (full token area) with holes cut for
   * each covered cell.
   *
   * @param {Token} token       The big token
   * @param {Set<string>} holeCells  Set of cell keys to punch out
   */
  _applyBigTokenHoleMask(token, holeCells) {
    // Skip if this token already has a pie-slice mask (same-size overlap
    // takes priority; this shouldn't happen due to data structure design)
    if (this.maskData.has(token.id)) return;

    const grid = canvas.grid;
    if (!grid) return;

    const tokenW = token.w;
    const tokenH = token.h;
    const tokenDocX = token.document.x;
    const tokenDocY = token.document.y;

    // === MASK 1: Token container ===
    // Draw the full token area, then cut out each hole cell
    const tokenMaskGfx = new PIXI.Graphics();
    tokenMaskGfx.beginFill(0xffffff);
    tokenMaskGfx.drawRect(0, 0, tokenW, tokenH);
    tokenMaskGfx.endFill();

    // Cut holes: each holeCells entry is a cell key "i,j"
    // We need the pixel bounds of that cell relative to the token's local coords
    tokenMaskGfx.beginHole();
    for (const cellKey of holeCells) {
      const [ci, cj] = cellKey.split(",").map(Number);
      const cellTopLeft = grid.getTopLeftPoint({ i: ci, j: cj });
      // Convert to token-local coordinates
      const localX = cellTopLeft.x - tokenDocX;
      const localY = cellTopLeft.y - tokenDocY;
      const cellW = grid.sizeX ?? grid.size;
      const cellH = grid.sizeY ?? grid.size;
      tokenMaskGfx.drawRect(localX, localY, cellW, cellH);
    }
    tokenMaskGfx.endHole();

    const tokenOriginalMask = token.mask;
    const tokenOriginalHitArea = token.hitArea;
    token.addChild(tokenMaskGfx);
    token.mask = tokenMaskGfx;
    token.hitArea = new PIXI.Rectangle(0, 0, tokenW, tokenH);

    // === MASK 2: Mesh ===
    const mesh = token.mesh;
    let meshMaskGfx = null;
    let meshOriginalMask = null;

    if (mesh) {
      const tex = mesh.texture;
      const texW = tex?.width ?? 1;
      const texH = tex?.height ?? 1;

      meshMaskGfx = new PIXI.Graphics();
      // Full token area in texture space (anchor 0.5, 0.5 → centre is 0,0)
      meshMaskGfx.beginFill(0xffffff);
      meshMaskGfx.drawRect(-texW / 2, -texH / 2, texW, texH);
      meshMaskGfx.endFill();

      // Cut holes in texture space
      // We need to map cell pixel coords → texture coords
      // mesh maps tokenDocX..tokenDocX+tokenW → -texW/2..texW/2
      const scaleToTexX = texW / tokenW;
      const scaleToTexY = texH / tokenH;

      meshMaskGfx.beginHole();
      for (const cellKey of holeCells) {
        const [ci, cj] = cellKey.split(",").map(Number);
        const cellTopLeft = grid.getTopLeftPoint({ i: ci, j: cj });
        const localX = cellTopLeft.x - tokenDocX;
        const localY = cellTopLeft.y - tokenDocY;
        const cellW = grid.sizeX ?? grid.size;
        const cellH = grid.sizeY ?? grid.size;
        // Convert to texture space
        const texLocalX = localX * scaleToTexX - texW / 2;
        const texLocalY = localY * scaleToTexY - texH / 2;
        meshMaskGfx.drawRect(texLocalX, texLocalY, cellW * scaleToTexX, cellH * scaleToTexY);
      }
      meshMaskGfx.endHole();

      meshOriginalMask = mesh.mask;
      mesh.addChild(meshMaskGfx);
      mesh.mask = meshMaskGfx;
    }

    this.maskData.set(token.id, {
      meshMaskGfx,
      meshOriginalMask,
      tokenMaskGfx,
      tokenOriginalMask,
      tokenOriginalHitArea,
    });

    // Highlight for big token: show only the visible portion (exclude holes)
    const hlGfx = new PIXI.Graphics();
    hlGfx.eventMode = "none";
    hlGfx.visible = false;

    const tx = tokenDocX;
    const ty = tokenDocY;
    const cellW = grid.sizeX ?? grid.size;
    const cellH = grid.sizeY ?? grid.size;
    const allCells = getTokenCellKeys(token);

    // Draw highlight only on non-hole cells
    for (const ck of allCells) {
      if (holeCells.has(ck)) continue;
      const [ci, cj] = ck.split(",").map(Number);
      const cellTopLeft = grid.getTopLeftPoint({ i: ci, j: cj });
      hlGfx.beginFill(0xffff00, 0.15);
      hlGfx.drawRect(cellTopLeft.x, cellTopLeft.y, cellW, cellH);
      hlGfx.endFill();
      hlGfx.lineStyle(2, 0xffff00, 0.6);
      hlGfx.drawRect(cellTopLeft.x, cellTopLeft.y, cellW, cellH);
      hlGfx.lineStyle(0);
    }

    this.overlayContainer.addChild(hlGfx);
    this.highlights.set(token.id, hlGfx);
  }

  /**
   * Create a simple rectangular highlight for a managed token that doesn't
   * need any masking (e.g. a lone 1×1 sitting on a 2×2 big token).
   * These tokens render normally but need a hover highlight.
   */
  _createSimpleHighlight(token) {
    const hlGfx = new PIXI.Graphics();
    hlGfx.eventMode = "none";
    hlGfx.visible = false;

    const tx = token.document.x;
    const ty = token.document.y;
    const tw = token.w;
    const th = token.h;

    hlGfx.beginFill(0xffff00, 0.22);
    hlGfx.drawRect(tx, ty, tw, th);
    hlGfx.endFill();
    hlGfx.lineStyle(2, 0xffff00, 0.6);
    hlGfx.drawRect(tx, ty, tw, th);

    this.overlayContainer.addChild(hlGfx);
    this.highlights.set(token.id, hlGfx);
  }

  _clearAllVisuals() {
    // Remove masks and restore originals
    for (const [tokenId, data] of this.maskData) {
      const token = canvas.tokens?.get(tokenId);

      // Restore token container mask and hitArea
      if (token) {
        if (token.mask === data.tokenMaskGfx) {
          token.mask = data.tokenOriginalMask ?? null;
        }
        if (data.tokenMaskGfx?.parent === token) {
          token.removeChild(data.tokenMaskGfx);
        }
        // Restore original hitArea (likely null/undefined = use bounds)
        token.hitArea = data.tokenOriginalHitArea ?? null;
      }
      if (data.tokenMaskGfx && !data.tokenMaskGfx.destroyed) {
        data.tokenMaskGfx.destroy({ children: true });
      }

      // Restore mesh mask
      if (token?.mesh && data.meshMaskGfx) {
        const mesh = token.mesh;
        if (mesh.mask === data.meshMaskGfx) {
          mesh.mask = data.meshOriginalMask ?? null;
        }
        if (data.meshMaskGfx.parent === mesh) {
          mesh.removeChild(data.meshMaskGfx);
        }
      }
      if (data.meshMaskGfx && !data.meshMaskGfx.destroyed) {
        data.meshMaskGfx.destroy({ children: true });
      }
    }
    this.maskData.clear();

    // Remove highlights
    for (const [, hlGfx] of this.highlights) {
      if (hlGfx.parent) hlGfx.parent.removeChild(hlGfx);
      if (!hlGfx.destroyed) hlGfx.destroy({ children: true });
    }
    this.highlights.clear();
    this.hoveredTokenId = null;
  }

  /* ── hover detection ─────────────────────────────────── */

  /**
   * Determine which managed token the mouse is over.
   *
   * For pie-sliced groups (same-size tokens sharing the same origin):
   *   → Determine slice by angle from the group's centre.
   *
   * For big tokens with hole-masks:
   *   → If the cursor is on one of its cells but NOT a hole-cell, it's
   *     hovering over the big token.
   *
   * @param {Point} canvasPos – position in canvas coordinates
   */
  onMouseMove(canvasPos) {
    if (!this.active || this.managedTokenIds.size === 0) return;

    const found = this._resolveTokenAtPosition(canvasPos);

    if (found) {
      this._setHighlight(found.id);
    } else {
      this._clearHighlight();
    }
  }

  /**
   * Resolve which managed token the given canvas position corresponds to.
   * @param {Point} canvasPos
   * @returns {Token|null}
   */
  _resolveTokenAtPosition(canvasPos) {
    const grid = canvas.grid;
    if (!grid || grid.isGridless) return null;

    const offset = grid.getOffset(canvasPos);
    const key = `${offset.i},${offset.j}`;

    // 1) Check pie-slice groups – if a group owns this cell, do angle detection
    for (const group of this.pieGroups) {
      const groupTokens = group.tokens;
      const groupToken = groupTokens[0];
      const groupCellKeys = getTokenCellKeys(groupToken);
      if (groupCellKeys.includes(key)) {
        // Mouse is on a cell that belongs to this pie-slice group
        const total = groupTokens.length;
        groupTokens.sort((a, b) => a.id.localeCompare(b.id));

        // Centre of the pie is the centre of the entire token area
        const tokenDocX = groupToken.document.x;
        const tokenDocY = groupToken.document.y;
        const pieCx = tokenDocX + groupToken.w / 2;
        const pieCy = tokenDocY + groupToken.h / 2;

        const dx = canvasPos.x - pieCx;
        const dy = canvasPos.y - pieCy;
        const angle = Math.atan2(dy, dx);

        const sliceIdx = getSliceIndexForAngle(angle, total);
        return groupTokens[sliceIdx] ?? null;
      }
    }

    // 2) Check big-token hole masks – if cursor is on a big token's cell but
    //    NOT a hole cell, then the big token is hovered.
    for (const [tokenId, info] of this.bigTokenMasks) {
      const token = canvas.tokens?.get(tokenId);
      if (!token) continue;
      const allCells = getTokenCellKeys(token);
      if (allCells.includes(key) && !info.holeCells.has(key)) {
        return token;
      }
    }

    // 3) Fallback: check for lone managed tokens on this cell (e.g. a single
    //    small token sitting on a big token, not pie-sliced, not a big token).
    //    These are the smallest tokens on the cell; pick the first one found.
    const cellTokens = this.overlapMap.get(key);
    if (cellTokens) {
      for (const t of cellTokens) {
        if (this.managedTokenIds.has(t.id) && !this.maskData.has(t.id)) {
          return t;
        }
      }
    }

    return null;
  }

  _setHighlight(tokenId) {
    if (this.hoveredTokenId === tokenId) return;
    this._clearHighlight();
    this.hoveredTokenId = tokenId;
    const hl = this.highlights.get(tokenId);
    if (hl) hl.visible = true;
  }

  _clearHighlight() {
    if (this.hoveredTokenId) {
      const hl = this.highlights.get(this.hoveredTokenId);
      if (hl) hl.visible = false;
      this.hoveredTokenId = null;
    }
  }

  /* ── click handlers ──────────────────────────────────── */

  /**
   * Attempt to select (control) the hovered token on left click.
   * @returns {boolean} true if we intercepted
   */
  onLeftClick(event) {
    if (!this.active || !this.hoveredTokenId) return false;
    const token = canvas.tokens.get(this.hoveredTokenId);
    if (!token) return false;

    const isShift = event?.shiftKey ?? event?.data?.originalEvent?.shiftKey ?? false;
    token.control({ releaseOthers: !isShift });
    return true;
  }

  /**
   * Target the hovered token on right click.
   * @returns {boolean} true if we intercepted
   */
  onRightClick(event) {
    if (!this.active || !this.hoveredTokenId) return false;
    const token = canvas.tokens.get(this.hoveredTokenId);
    if (!token) return false;

    const isTargeted = token.isTargeted;
    token.setTarget(!isTargeted, { releaseOthers: false });
    return true;
  }

  /* ── check if position is over a stacked cell ────────── */

  /**
   * Return the managed token under the mouse, using the same logic as
   * onMouseMove (pie-slice angle detection + big-token hole awareness).
   * @param {Point} canvasPos
   * @returns {Token|null}
   */
  getTokenAtPosition(canvasPos) {
    if (!this.active) return null;
    return this._resolveTokenAtPosition(canvasPos);
  }

  /* ── keyboard popup ──────────────────────────────────── */

  /**
   * Get all managed tokens whose area covers the current mouse cell.
   * Used by the popup (keyboard shortcut) to list all tokens the user
   * can pick from.
   */
  getTokensAtMouse() {
    if (!this.active || !canvas?.ready) return [];
    const grid = canvas.grid;
    if (!grid || grid.isGridless) return [];

    const pos = canvas.mousePosition;
    const offset = grid.getOffset(pos);
    const key = `${offset.i},${offset.j}`;

    // Collect all managed tokens whose cells contain this key
    const result = [];
    const seen = new Set();
    for (const tokenId of this.managedTokenIds) {
      if (seen.has(tokenId)) continue;
      const token = canvas.tokens?.get(tokenId);
      if (!token) continue;
      const cells = getTokenCellKeys(token);
      if (cells.includes(key)) {
        result.push(token);
        seen.add(tokenId);
      }
    }
    return result;
  }

  openPopup() {
    if (this.popupOpen) {
      this.closePopup();
      return;
    }
    const tokens = this.getTokensAtMouse();
    if (tokens.length === 0) return;

    this.popupOpen = true;
    this._renderPopupHTML(tokens);
  }

  closePopup() {
    this.popupOpen = false;
    const el = document.getElementById("otv-token-list-popup");
    if (el) el.remove();
    // Remove the outside-click handler so it doesn't interfere with future popups
    if (this._popupCloseHandler) {
      document.removeEventListener("pointerdown", this._popupCloseHandler, true);
      this._popupCloseHandler = null;
    }
  }

  _renderPopupHTML(tokens) {
    // Remove any existing popup
    this.closePopup();
    this.popupOpen = true;

    const popup = document.createElement("div");
    popup.id = "otv-token-list-popup";

    // Title
    const titleEl = document.createElement("div");
    titleEl.className = "otv-popup-title";
    titleEl.textContent = game.i18n.localize("OTV.ListPopup.Title");
    popup.appendChild(titleEl);

    // Sort tokens by name for the list
    const sortedTokens = [...tokens].sort((a, b) => (a.name ?? "").localeCompare(b.name ?? ""));

    for (const token of sortedTokens) {
      const tokenId = token.id;
      const row = document.createElement("div");
      row.className = "otv-popup-row";
      if (token.controlled) row.classList.add("controlled");
      if (token.isTargeted) row.classList.add("targeted");

      // Token image
      const img = document.createElement("img");
      img.className = "otv-popup-img";
      img.src = token.document.texture?.src ?? "icons/svg/mystery-man.svg";
      img.alt = token.name ?? "";
      row.appendChild(img);

      // Name
      const nameEl = document.createElement("span");
      nameEl.className = "otv-popup-name";
      nameEl.textContent = token.name || "???";
      row.appendChild(nameEl);

      // Buttons container
      const btns = document.createElement("div");
      btns.className = "otv-popup-buttons";

      // Helper: get a fresh token reference by id (avoids stale closures
      // if FVTT re-draws tokens between popup open and click).
      const getToken = () => canvas.tokens?.get(tokenId);

      // Select button
      const selectBtn = document.createElement("button");
      selectBtn.className = "otv-popup-btn select-btn";
      selectBtn.textContent = game.i18n.localize("OTV.ListPopup.Select");
      selectBtn.addEventListener("click", (e) => {
        e.stopPropagation();
        // Close popup first to avoid DOM/event-ordering interference
        this.closePopup();
        const t = getToken();
        if (t) t.control({ releaseOthers: !e.shiftKey });
      });
      btns.appendChild(selectBtn);

      // Target button
      const targetBtn = document.createElement("button");
      targetBtn.className = "otv-popup-btn target-btn";
      targetBtn.textContent = game.i18n.localize("OTV.ListPopup.Target");
      targetBtn.addEventListener("click", (e) => {
        e.stopPropagation();
        const t = getToken();
        if (t) {
          t.setTarget(!t.isTargeted, { releaseOthers: false });
          row.classList.toggle("targeted", t.isTargeted);
        }
      });
      btns.appendChild(targetBtn);

      row.appendChild(btns);

      // Click on the row → select
      row.addEventListener("click", (e) => {
        if (e.target.tagName === "BUTTON") return; // Don't duplicate button clicks
        this.closePopup();
        const t = getToken();
        if (t) t.control({ releaseOthers: !e.shiftKey });
      });

      popup.appendChild(row);
    }

    // Position near the mouse cursor
    const clientPos = canvas.clientCoordinatesFromCanvas(canvas.mousePosition);
    const left = Math.min(clientPos.x + 10, window.innerWidth - 340);
    const top = Math.min(clientPos.y + 10, window.innerHeight - 420);
    popup.style.left = `${Math.max(0, left)}px`;
    popup.style.top = `${Math.max(0, top)}px`;

    document.body.appendChild(popup);

    // Close on outside click (with a small delay to avoid the trigger click)
    const closeHandler = (e) => {
      if (!popup.contains(e.target)) {
        this.closePopup();
      }
    };
    // Store reference so closePopup() can remove it
    this._popupCloseHandler = closeHandler;
    setTimeout(() => {
      document.addEventListener("pointerdown", closeHandler, true);
    }, 150);
  }
}

/* ──────────────────────────────────────────────────────────────────────────────
   Module initialisation & hooks
   ────────────────────────────────────────────────────────────────────────────── */

let manager = null;

Hooks.once("init", () => {
  // Register settings
  game.settings.register(MODULE_ID, "enabled", {
    name: "OTV.Settings.Enabled.Name",
    hint: "OTV.Settings.Enabled.Hint",
    scope: "client",
    config: true,
    type: Boolean,
    default: true,
    onChange: () => {
      if (canvas?.ready) {
        if (game.settings.get(MODULE_ID, "enabled")) {
          manager?.init();
        } else {
          manager?.tearDown();
        }
      }
    },
  });

  game.settings.register(MODULE_ID, "minOverlap", {
    name: "OTV.Settings.MinOverlap.Name",
    hint: "OTV.Settings.MinOverlap.Hint",
    scope: "world",
    config: true,
    type: Number,
    default: 2,
    range: { min: 2, max: 20, step: 1 },
    onChange: () => manager?.scheduleRefresh(),
  });

  game.settings.register(MODULE_ID, "listKey", {
    name: "OTV.Settings.ListKey.Name",
    hint: "OTV.Settings.ListKey.Hint",
    scope: "client",
    config: true,
    type: String,
    default: "KeyL",
    onChange: () => {},
  });
});

/* ── Canvas ready → init ──────────────────────────────── */

Hooks.on("canvasReady", () => {
  if (!game.settings.get(MODULE_ID, "enabled")) return;
  if (!manager) manager = new OverlapTokenManager();
  manager.init();
});

Hooks.on("canvasTearDown", () => {
  manager?.tearDown();
});

/* ── Token lifecycle → refresh ───────────────────────── */

const refreshEvents = [
  "createToken", "updateToken", "deleteToken",
];

for (const hookName of refreshEvents) {
  Hooks.on(hookName, () => manager?.scheduleRefresh());
}

// Also refresh on token position changes (refreshToken fires on every visual refresh)
Hooks.on("refreshToken", (token, flags) => {
  // Only trigger if position-related flags changed
  if (flags?.refreshPosition || flags?.refreshSize) {
    manager?.scheduleRefresh();
  }
});

/* ── Mouse move handler ──────────────────────────────── */

Hooks.on("canvasReady", () => {
  if (!canvas?.stage) return;
  canvas.stage.on("pointermove", (event) => {
    if (!manager?.active) return;
    const pos = event.getLocalPosition(canvas.stage);
    manager.onMouseMove(pos);
  });
});

/* ── Click interception via libWrapper-style monkey patching ──── */
// We wrap the TokenLayer prototype methods once at init, not on every canvasReady

let _wrappedClicks = false;

Hooks.once("canvasReady", () => {
  if (_wrappedClicks) return;
  _wrappedClicks = true;

  // Wrap Token._onClickLeft to intercept clicks on pie-sliced tokens
  const origTokenClickLeft = CONFIG.Token.objectClass.prototype._onClickLeft;
  CONFIG.Token.objectClass.prototype._onClickLeft = function (event) {
    if (manager?.active && manager.managedTokenIds.size > 0) {
      // Check if this token is managed (part of any overlap group)
      if (manager.managedTokenIds.has(this.id)) {
        const origEvent = event?.data?.originalEvent ?? event;
        const pos = event?.interactionData?.origin ?? canvas.mousePosition;
        const target = manager.getTokenAtPosition(pos);
        if (target) {
          const isShift = origEvent?.shiftKey ?? false;
          target.control({ releaseOthers: !isShift });
          return;
        }
      }
    }
    return origTokenClickLeft.call(this, event);
  };

  // Wrap Token._onClickRight for targeting
  const origTokenClickRight = CONFIG.Token.objectClass.prototype._onClickRight;
  CONFIG.Token.objectClass.prototype._onClickRight = function (event) {
    if (manager?.active && manager.managedTokenIds.size > 0) {
      if (manager.managedTokenIds.has(this.id)) {
        const pos = event?.interactionData?.origin ?? canvas.mousePosition;
        const target = manager.getTokenAtPosition(pos);
        if (target) {
          target.setTarget(!target.isTargeted, { releaseOthers: false });
          return;
        }
      }
    }
    return origTokenClickRight.call(this, event);
  };
});

/* ── Keyboard handler (popup list) ───────────────────── */

let _keyListenerAdded = false;
Hooks.on("canvasReady", () => {
  if (_keyListenerAdded) return;
  _keyListenerAdded = true;

  document.addEventListener("keydown", (event) => {
    if (!manager?.active) return;
    if (event.repeat) return;

    // Don't intercept if user is typing in an input field
    const tag = event.target?.tagName;
    if (tag === "INPUT" || tag === "TEXTAREA" || tag === "SELECT") return;

    const listKey = game.settings.get(MODULE_ID, "listKey");
    if (event.code === listKey) {
      const tokens = manager.getTokensAtMouse();
      if (tokens.length > 0) {
        event.preventDefault();
        event.stopPropagation();
        manager.openPopup();
      }
    }

    // Escape closes popup
    if (event.code === "Escape" && manager.popupOpen) {
      manager.closePopup();
    }
  }, true); // capture phase so we get it before Foundry
});

console.log(`${MODULE_ID} | Module loaded`);
