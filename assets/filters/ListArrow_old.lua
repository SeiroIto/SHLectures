--[[
  A lua filter for list-like tables with ==> aligned
  E.g., 
    A is A         	==> B is cool
    B is B and C	==> C is cool
]]
-- Created by ChatGPT Plus, 2026 July 07, 05:19
-- Rev1 by ChatGPT Plus, 2026 July 07, 06:44 (+ Column width.)
-- Rev2 by ChatGPT Plus, 2026 July 07, 09:01 (+ Fragment index, scale factor, arrow cell.)
-- Rev3 by ChatGPT Plus, 2026 July 07, 09:35 (+ scount)
-- ============================================================================
-- ListArrow.lua
--
-- Pandoc/Quarto filter for ListArrow fenced divs.
-- Creates a three-column list with an automatically inserted arrow. Fragment index incorporated.
--
-- Usage:
--
-- ::::: {.ListArrow
--     left=60
--    arrow=5
--    right=35
--    scale=90
--   scount=5}
-- :::: {.arrow-row left-overlay=1 arrow-overlay=4 right-overlay=4}
-- ::: {.arrow-left}
-- Left text
-- :::
-- ::: {.arrow}
-- ÅÀ
-- :::
-- ::: {.arrow-right}
-- Right text
-- :::
-- ::::
-- :::::
--
-- Parameters
--   left    Relative width of the left column.
--   arrow   Relative width of the middle column.
--   right   Relative width of the right column.
--   scale   Overall width of the ListArrow block.
--   scount  Starting fragment offset.
-- Numeric values are interpreted as percentages by default.
-- Example:
--   left=60
--   arrow=5
--   right=35
--   scale=90
-- produces
--   left  = 60% Å~ 90% = 54%
--   arrow =  5% Å~ 90% = 4.5%
--   right = 35% Å~ 90% = 31.5%
-- or
--   left=60
--   arrow=5
--   right=35
--   scale=9cm
-- produces
--   left  = 60% Å~ 9cm = 5.4cm
--   arrow =  5% Å~ 9cm = 0.45cm
--   right = 35% Å~ 9cm = 3.15cm
-- If scale is omitted, it defaults to 100%.
-- Example:
--   scount=5
-- together with
--    left-overlay=1
--   arrow-overlay=4
--   right-overlay=4
-- produces fragment indices
--   left  = 6
--   arrow = 9
--   right = 9
-- ============================================================================

------------------------------------------------------------------------------
-- Current ListArrow fragment offset.
------------------------------------------------------------------------------

local current_scount = 0

------------------------------------------------------------------------------
-- Convert a numeric value to a CSS percentage.
------------------------------------------------------------------------------

local function css_length(x)
  if not x or x == "" then
    return nil
  end

  if x:match("^%-?%d+%.?%d*$") then
    return x .. "%"
  end

  return x
end

------------------------------------------------------------------------------
-- Apply ListArrow fragment offset.
------------------------------------------------------------------------------

local function overlay_index(idx)
  if not idx then
    return nil
  end

  return tostring(current_scount + tonumber(idx))
end

------------------------------------------------------------------------------
-- Add a reveal.js fragment.
------------------------------------------------------------------------------

local function add_fragment(div, idx)
  if div and idx then
    div.classes:insert("fragment")
    div.attributes["data-fragment-index"] = idx
  end
end

------------------------------------------------------------------------------
-- Main filter.
------------------------------------------------------------------------------

function Div(el)

  --------------------------------------------------------------------------
  -- ListArrow container.
  --------------------------------------------------------------------------

  if el.classes:includes("ListArrow") then

    local left   = css_length(el.attributes.left)   or "45%"
    local arrow  = css_length(el.attributes.arrow)  or "5%"
    local right  = css_length(el.attributes.right)  or "50%"
    local scale  = css_length(el.attributes.scale)  or "100%"
    current_scount = tonumber(el.attributes.scount) or 0

    el.attributes.left   = nil
    el.attributes.arrow  = nil
    el.attributes.right  = nil
    el.attributes.scale  = nil
    el.attributes.scount = nil

    local style = el.attributes.style or ""

    if style ~= "" and not style:match(";%s*$") then
      style = style .. "; "
    end

    style = style ..
      "--left-width:"  .. left  .. ";" ..
      "--arrow-width:" .. arrow .. ";" ..
      "--right-width:" .. right .. ";" ..
      "--list-width:"  .. scale .. ";"

    el.attributes.style = style

    return el
  end

  --------------------------------------------------------------------------
  -- Arrow row.
  --------------------------------------------------------------------------

  if el.classes:includes("arrow-row") then

    local left_idx  = overlay_index(el.attributes["left-overlay"])
    local arrow_idx = overlay_index(el.attributes["arrow-overlay"])
    local right_idx = overlay_index(el.attributes["right-overlay"])

    el.attributes["left-overlay"]  = nil
    el.attributes["arrow-overlay"] = nil
    el.attributes["right-overlay"] = nil

    local left_div
    local arrow_div
    local right_div

    for _, item in ipairs(el.content) do
      if item.t == "Div" then

        if item.classes:includes("arrow-left") then
          left_div = item

        elseif item.classes:includes("arrow") then
          arrow_div = item

        elseif item.classes:includes("arrow-right") then
          right_div = item

        end
      end
    end

    add_fragment(left_div,  left_idx)
    add_fragment(arrow_div, arrow_idx)
    add_fragment(right_div, right_idx)

    el.content = {
      left_div,
      arrow_div,
      right_div
    }

    return el
  end

  return nil
end
