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
-- Rev4 by ChatGPT Mini, 2026 July 07, 09:51 (+ debug)
-- ============================================================================
-- ListArrow.lua
--
-- Pandoc/Quarto filter for ListArrow fenced divs.
-- Creates a three-column list with an automatically inserted arrow. Fragment index incorporated.
--
-- Example:
-- ::::: {.ListArrow left=60 arrow=5 right=35 scount=6}
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
-- Fragment indices become:
--   left  = scount + left-overlay
--   arrow = scount + arrow-overlay
--   right = scount + right-overlay
-- If scount is omitted, it defaults to 0.
-- Width:
--   left, arrow, right are percentages.
--   scale controls the total width.
-- Example:
--   left=60 arrow=5 right=35 scale=90
-- means:
--   left  = 60% of 90%
--   arrow =  5% of 90%
--   right = 35% of 90%
-- ============================================================================

------------------------------------------------------------------------------
-- Convert numeric values into CSS percentages.
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
-- Add Reveal.js fragment attributes.
------------------------------------------------------------------------------
local function add_fragment(div, index)
  if div and index then
    div.classes:insert("fragment")
    div.attributes["data-fragment-index"] = tostring(index)
  end
end
------------------------------------------------------------------------------
-- Add offset to overlay number.
------------------------------------------------------------------------------
local function offset_overlay(value, scount)
  if not value then
    return nil
  end
  return tonumber(value) + scount
end
------------------------------------------------------------------------------
-- Process a ListArrow block.
------------------------------------------------------------------------------
function Div(el)
  if not el.classes:includes("ListArrow") then
    return nil
  end
  --------------------------------------------------------------------------
  -- Container settings
  --------------------------------------------------------------------------
  local left =
    css_length(el.attributes.left) or "45%"
  local arrow =
    css_length(el.attributes.arrow) or "5%"
  local right =
    css_length(el.attributes.right) or "50%"
  local scale =
    css_length(el.attributes.scale) or "100%"
  local scount =
    tonumber(el.attributes.scount) or 0
  el.attributes.left   = nil
  el.attributes.arrow  = nil
  el.attributes.right  = nil
  el.attributes.scale  = nil
  el.attributes.scount = nil
  local style = el.attributes.style or ""
  if style ~= "" and not style:match(";%s*$") then
    style = style .. ";"
  end
  style = style ..
    "--left-width:"  .. left  .. ";" ..
    "--arrow-width:" .. arrow .. ";" ..
    "--right-width:" .. right .. ";" ..
    "--list-width:"  .. scale .. ";"
  el.attributes.style = style
  --------------------------------------------------------------------------
  -- Process child arrow rows
  --------------------------------------------------------------------------
  for _, row in ipairs(el.content) do
    if row.t == "Div"
       and row.classes:includes("arrow-row") then
      local left_idx =
        offset_overlay(
          row.attributes["left-overlay"],
          scount
        )
      local arrow_idx =
        offset_overlay(
          row.attributes["arrow-overlay"],
          scount
        )
      local right_idx =
        offset_overlay(
          row.attributes["right-overlay"],
          scount
        )
      row.attributes["left-overlay"]  = nil
      row.attributes["arrow-overlay"] = nil
      row.attributes["right-overlay"] = nil
      for _, cell in ipairs(row.content) do
        if cell.t == "Div" then
          if cell.classes:includes("arrow-left") then
            add_fragment(cell, left_idx)
          elseif cell.classes:includes("arrow") then
            add_fragment(cell, arrow_idx)
          elseif cell.classes:includes("arrow-right") then
            add_fragment(cell, right_idx)
          end
        end
      end
    end
  end
  return el
end
