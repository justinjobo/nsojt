/*
  Highlight.js 10.0.0-beta.0 (ae18d9cd)
  License: BSD-3-Clause
  Copyright (c) 2006-2020, Ivan Sagalaev
*/
(function (global, factory) {
  typeof exports === 'object' && typeof module !== 'undefined' ? module.exports = factory() :
  typeof define === 'function' && define.amd ? define(factory) :
  (global = global || self, global.hljs = factory());
}(this, (function () { 'use strict';

  // https://github.com/substack/deep-freeze
  function deepFreeze (o) {
    Object.freeze(o);

    var objIsFunction = typeof o === 'function';

    Object.getOwnPropertyNames(o).forEach(function (prop) {
      if (o.hasOwnProperty(prop)
      && o[prop] !== null
      && (typeof o[prop] === "object" || typeof o[prop] === "function")
      // IE11 fix: https://github.com/highlightjs/highlight.js/issues/2318
      // TODO: remove in the future
      && (objIsFunction ? prop !== 'caller' && prop !== 'callee' && prop !== 'arguments' : true)
      && !Object.isFrozen(o[prop])) {
        deepFreeze(o[prop]);
      }
    });

    return o;
  }

  function escape(value) {
    return new RegExp(value.replace(/[-\/\\^$*+?.()|[\]{}]/g, '\\$&'), 'm');
  }

  function source(re) {
    // if it's a regex get it's source,
    // otherwise it's a string already so just return it
    return (re && re.source) || re;
  }

  function countMatchGroups(re) {
    return (new RegExp(re.toString() + '|')).exec('').length - 1;
  }

  function startsWith(re, lexeme) {
    var match = re && re.exec(lexeme);
    return match && match.index === 0;
  }

  // join logically computes regexps.join(separator), but fixes the
  // backreferences so they continue to match.
  // it also places each individual regular expression into it's own
  // match group, keeping track of the sequencing of those match groups
  // is currently an exercise for the caller. :-)
  function join(regexps, separator) {
    // backreferenceRe matches an open parenthesis or backreference. To avoid
    // an incorrect parse, it additionally matches the following:
    // - [...] elements, where the meaning of parentheses and escapes change
    // - other escape sequences, so we do not misparse escape sequences as
    //   interesting elements
    // - non-matching or lookahead parentheses, which do not capture. These
    //   follow the '(' with a '?'.
    var backreferenceRe = /\[(?:[^\\\]]|\\.)*\]|\(\??|\\([1-9][0-9]*)|\\./;
    var numCaptures = 0;
    var ret = '';
    for (var i = 0; i < regexps.length; i++) {
      numCaptures += 1;
      var offset = numCaptures;
      var re = source(regexps[i]);
      if (i > 0) {
        ret += separator;
      }
      ret += "(";
      while (re.length > 0) {
        var match = backreferenceRe.exec(re);
        if (match == null) {
          ret += re;
          break;
        }
        ret += re.substring(0, match.index);
        re = re.substring(match.index + match[0].length);
        if (match[0][0] == '\\' && match[1]) {
          // Adjust the backreference.
          ret += '\\' + String(Number(match[1]) + offset);
        } else {
          ret += match[0];
          if (match[0] == '(') {
            numCaptures++;
          }
        }
      }
      ret += ")";
    }
    return ret;
  }

  function escapeHTML(value) {
    return value.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;');
  }


  
  function inherit(parent) {  // inherit(parent, override_obj, override_obj, ...)
    var key;
    var result = {};
    var objects = Array.prototype.slice.call(arguments, 1);

    for (key in parent)
      result[key] = parent[key];
    objects.forEach(function(obj) {
      for (key in obj)
        result[key] = obj[key];
    });
    return result;
  }

  /* Stream merging */


  function tag(node) {
    return node.nodeName.toLowerCase();
  }


  function nodeStream(node) {
    var result = [];
    (function _nodeStream(node, offset) {
      for (var child = node.firstChild; child; child = child.nextSibling) {
        if (child.nodeType === 3)
          offset += child.nodeValue.length;
        else if (child.nodeType === 1) {
          result.push({
            event: 'start',
            offset: offset,
            node: child
          });
          offset = _nodeStream(child, offset);
          // Prevent void elements from having an end tag that would actually
          // double them in the output. There are more void elements in HTML
          // but we list only those realistically expected in code display.
          if (!tag(child).match(/br|hr|img|input/)) {
            result.push({
              event: 'stop',
              offset: offset,
              node: child
            });
          }
        }
      }
      return offset;
    })(node, 0);
    return result;
  }

  function mergeStreams(original, highlighted, value) {
    var processed = 0;
    var result = '';
    var nodeStack = [];

    function selectStream() {
      if (!original.length || !highlighted.length) {
        return original.length ? original : highlighted;
      }
      if (original[0].offset !== highlighted[0].offset) {
        return (original[0].offset < highlighted[0].offset) ? original : highlighted;
      }

      /*
      To avoid starting the stream just before it should stop the order is
      ensured that original always starts first and closes last:

      if (event1 == 'start' && event2 == 'start')
        return original;
      if (event1 == 'start' && event2 == 'stop')
        return highlighted;
      if (event1 == 'stop' && event2 == 'start')
        return original;
      if (event1 == 'stop' && event2 == 'stop')
        return highlighted;

      ... which is collapsed to:
      */
      return highlighted[0].event === 'start' ? original : highlighted;
    }

    function open(node) {
      function attr_str(a) {
        return ' ' + a.nodeName + '="' + escapeHTML(a.value).replace(/"/g, '&quot;') + '"';
      }
      result += '<' + tag(node) + [].map.call(node.attributes, attr_str).join('') + '>';
    }

    function close(node) {
      result += '</' + tag(node) + '>';
    }

    function render(event) {
      (event.event === 'start' ? open : close)(event.node);
    }

    while (original.length || highlighted.length) {
      var stream = selectStream();
      result += escapeHTML(value.substring(processed, stream[0].offset));
      processed = stream[0].offset;
      if (stream === original) {
        /*
        On any opening or closing tag of the original markup we first close
        the entire highlighted node stack, then render the original tag along
        with all the following original tags at the same offset and then
        reopen all the tags on the highlighted stack.
        */
        nodeStack.reverse().forEach(close);
        do {
          render(stream.splice(0, 1)[0]);
          stream = selectStream();
        } while (stream === original && stream.length && stream[0].offset === processed);
        nodeStack.reverse().forEach(open);
      } else {
        if (stream[0].event === 'start') {
          nodeStack.push(stream[0].node);
        } else {
          nodeStack.pop();
        }
        render(stream.splice(0, 1)[0]);
      }
    }
    return result + escapeHTML(value.substr(processed));
  }

  var utils = /*#__PURE__*/Object.freeze({
    __proto__: null,
    escapeHTML: escapeHTML,
    inherit: inherit,
    nodeStream: nodeStream,
    mergeStreams: mergeStreams
  });

  // Common regexps
  const IDENT_RE = '[a-zA-Z]\\w*';
  const UNDERSCORE_IDENT_RE = '[a-zA-Z_]\\w*';
  const NUMBER_RE = '\\b\\d+(\\.\\d+)?';
  const C_NUMBER_RE = '(-?)(\\b0[xX][a-fA-F0-9]+|(\\b\\d+(\\.\\d*)?|\\.\\d+)([eE][-+]?\\d+)?)'; // 0x..., 0..., decimal, float
  const BINARY_NUMBER_RE = '\\b(0b[01]+)'; // 0b...
  const RE_STARTERS_RE = '!|!=|!==|%|%=|&|&&|&=|\\*|\\*=|\\+|\\+=|,|-|-=|/=|/|:|;|<<|<<=|<=|<|===|==|=|>>>=|>>=|>=|>>>|>>|>|\\?|\\[|\\{|\\(|\\^|\\^=|\\||\\|=|\\|\\||~';

  // Common modes
  const BACKSLASH_ESCAPE = {
    begin: '\\\\[\\s\\S]', relevance: 0
  };
  const APOS_STRING_MODE = {
    className: 'string',
    begin: '\'', end: '\'',
    illegal: '\\n',
    contains: [BACKSLASH_ESCAPE]
  };
  const QUOTE_STRING_MODE = {
    className: 'string',
    begin: '"', end: '"',
    illegal: '\\n',
    contains: [BACKSLASH_ESCAPE]
  };
  const PHRASAL_WORDS_MODE = {
    begin: /\b(a|an|the|are|I'm|isn't|don't|doesn't|won't|but|just|should|pretty|simply|enough|gonna|going|wtf|so|such|will|you|your|they|like|more)\b/
  };
  const COMMENT = function (begin, end, inherits) {
    var mode = inherit(
      {
        className: 'comment',
        begin: begin, end: end,
        contains: []
      },
      inherits || {}
    );
    mode.contains.push(PHRASAL_WORDS_MODE);
    mode.contains.push({
      className: 'doctag',
      begin: '(?:TODO|FIXME|NOTE|BUG|XXX):',
      relevance: 0
    });
    return mode;
  };
  const C_LINE_COMMENT_MODE = COMMENT('//', '$');
  const C_BLOCK_COMMENT_MODE = COMMENT('/\\*', '\\*/');
  const HASH_COMMENT_MODE = COMMENT('#', '$');
  const NUMBER_MODE = {
    className: 'number',
    begin: NUMBER_RE,
    relevance: 0
  };
  const C_NUMBER_MODE = {
    className: 'number',
    begin: C_NUMBER_RE,
    relevance: 0
  };
  const BINARY_NUMBER_MODE = {
    className: 'number',
    begin: BINARY_NUMBER_RE,
    relevance: 0
  };
  const CSS_NUMBER_MODE = {
    className: 'number',
    begin: NUMBER_RE + '(' +
      '%|em|ex|ch|rem'  +
      '|vw|vh|vmin|vmax' +
      '|cm|mm|in|pt|pc|px' +
      '|deg|grad|rad|turn' +
      '|s|ms' +
      '|Hz|kHz' +
      '|dpi|dpcm|dppx' +
      ')?',
    relevance: 0
  };
  const REGEXP_MODE = {
    // this outer rule makes sure we actually have a WHOLE regex and not simply
    // an expression such as:
    //
    //     3 / something
    //
    // (which will then blow up when regex's `illegal` sees the newline)
    begin: /(?=\/[^\/\n]*\/)/,
    contains: [{
      className: 'regexp',
      begin: /\//, end: /\/[gimuy]*/,
      illegal: /\n/,
      contains: [
        BACKSLASH_ESCAPE,
        {
          begin: /\[/, end: /\]/,
          relevance: 0,
          contains: [BACKSLASH_ESCAPE]
        }
      ]
    }]
  };
  const TITLE_MODE = {
    className: 'title',
    begin: IDENT_RE,
    relevance: 0
  };
  const UNDERSCORE_TITLE_MODE = {
    className: 'title',
    begin: UNDERSCORE_IDENT_RE,
    relevance: 0
  };
  const METHOD_GUARD = {
    // excludes method names from keyword processing
    begin: '\\.\\s*' + UNDERSCORE_IDENT_RE,
    relevance: 0
  };

  var MODES = /*#__PURE__*/Object.freeze({
    __proto__: null,
    IDENT_RE: IDENT_RE,
    UNDERSCORE_IDENT_RE: UNDERSCORE_IDENT_RE,
    NUMBER_RE: NUMBER_RE,
    C_NUMBER_RE: C_NUMBER_RE,
    BINARY_NUMBER_RE: BINARY_NUMBER_RE,
    RE_STARTERS_RE: RE_STARTERS_RE,
    BACKSLASH_ESCAPE: BACKSLASH_ESCAPE,
    APOS_STRING_MODE: APOS_STRING_MODE,
    QUOTE_STRING_MODE: QUOTE_STRING_MODE,
    PHRASAL_WORDS_MODE: PHRASAL_WORDS_MODE,
    COMMENT: COMMENT,
    C_LINE_COMMENT_MODE: C_LINE_COMMENT_MODE,
    C_BLOCK_COMMENT_MODE: C_BLOCK_COMMENT_MODE,
    HASH_COMMENT_MODE: HASH_COMMENT_MODE,
    NUMBER_MODE: NUMBER_MODE,
    C_NUMBER_MODE: C_NUMBER_MODE,
    BINARY_NUMBER_MODE: BINARY_NUMBER_MODE,
    CSS_NUMBER_MODE: CSS_NUMBER_MODE,
    REGEXP_MODE: REGEXP_MODE,
    TITLE_MODE: TITLE_MODE,
    UNDERSCORE_TITLE_MODE: UNDERSCORE_TITLE_MODE,
    METHOD_GUARD: METHOD_GUARD
  });

  // keywords that should have no default relevance value
  var COMMON_KEYWORDS = 'of and for in not or if then'.split(' ');

  // compilation

  function compileLanguage(language) {

    function langRe(value, global) {
      return new RegExp(
        source(value),
        'm' + (language.case_insensitive ? 'i' : '') + (global ? 'g' : '')
      );
    }

    function buildModeRegex(mode) {

      var matchIndexes = {};
      var matcherRe;
      var regexes = [];
      var matcher = {};
      var matchAt = 1;

      function addRule(rule, re) {
        matchIndexes[matchAt] = rule;
        regexes.push([rule, re]);
        matchAt += countMatchGroups(re) + 1;
      }

      mode.contains.forEach(term => addRule(term, term.begin));

      if (mode.terminator_end)
        addRule("end", mode.terminator_end);
      if (mode.illegal)
        addRule("illegal", mode.illegal);

      var terminators = regexes.map(el => el[1]);
      matcherRe = langRe(join(terminators, '|'), true);

      matcher.lastIndex = 0;
      matcher.exec = function(s) {
        var rule;

        if( regexes.length === 0) return null;

        matcherRe.lastIndex = matcher.lastIndex;
        var match = matcherRe.exec(s);
        if (!match) { return null; }

        for(var i = 0; i<match.length; i++) {
          if (match[i] != undefined && matchIndexes[i]) {
            rule = matchIndexes[i];
            break;
          }
        }

        // illegal or end match
        if (typeof rule === "string") {
          match.type = rule;
          match.extra = [mode.illegal, mode.terminator_end];
        } else {
          match.type = "begin";
          match.rule = rule;
        }
        return match;
      };

      return matcher;
    }

    function compileMode(mode, parent) {
      if (mode.compiled)
        return;
      mode.compiled = true;

      mode.keywords = mode.keywords || mode.beginKeywords;
      if (mode.keywords)
        mode.keywords = compileKeywords(mode.keywords, language.case_insensitive);

      mode.lexemesRe = langRe(mode.lexemes || /\w+/, true);

      if (parent) {
        if (mode.beginKeywords) {
          mode.begin = '\\b(' + mode.beginKeywords.split(' ').join('|') + ')\\b';
        }
        if (!mode.begin)
          mode.begin = /\B|\b/;
        mode.beginRe = langRe(mode.begin);
        if (mode.endSameAsBegin)
          mode.end = mode.begin;
        if (!mode.end && !mode.endsWithParent)
          mode.end = /\B|\b/;
        if (mode.end)
          mode.endRe = langRe(mode.end);
        mode.terminator_end = source(mode.end) || '';
        if (mode.endsWithParent && parent.terminator_end)
          mode.terminator_end += (mode.end ? '|' : '') + parent.terminator_end;
      }
      if (mode.illegal)
        mode.illegalRe = langRe(mode.illegal);
      if (mode.relevance == null)
        mode.relevance = 1;
      if (!mode.contains) {
        mode.contains = [];
      }
      mode.contains = [].concat(...mode.contains.map(function(c) {
        return expand_or_clone_mode(c === 'self' ? mode : c);
      }));
      mode.contains.forEach(function(c) {compileMode(c, mode);});

      if (mode.starts) {
        compileMode(mode.starts, parent);
      }

      mode.terminators = buildModeRegex(mode);
    }

    // self is not valid at the top-level
    if (language.contains && language.contains.includes('self')) {
      throw new Error("ERR: contains `self` is not supported at the top-level of a language.  See documentation.")
    }
    compileMode(language);
  }

  function dependencyOnParent(mode) {
    if (!mode) return false;

    return mode.endsWithParent || dependencyOnParent(mode.starts);
  }

  function expand_or_clone_mode(mode) {
    if (mode.variants && !mode.cached_variants) {
      mode.cached_variants = mode.variants.map(function(variant) {
        return inherit(mode, {variants: null}, variant);
      });
    }

    // EXPAND
    // if we have variants then essentially "replace" the mode with the variants
    // this happens in compileMode, where this function is called from
    if (mode.cached_variants)
      return mode.cached_variants;

    // CLONE
    // if we have dependencies on parents then we need a unique
    // instance of ourselves, so we can be reused with many
    // different parents without issue
    if (dependencyOnParent(mode))
      return inherit(mode, { starts: mode.starts ? inherit(mode.starts) : null });

    if (Object.isFrozen(mode))
      return inherit(mode);

    // no special dependency issues, just return ourselves
    return mode;
  }


  // keywords

  function compileKeywords(rawKeywords, case_insensitive) {
    var compiled_keywords = {};

    if (typeof rawKeywords === 'string') { // string
      splitAndCompile('keyword', rawKeywords);
    } else {
      Object.keys(rawKeywords).forEach(function (className) {
        splitAndCompile(className, rawKeywords[className]);
      });
    }
  return compiled_keywords;

  // ---

  function splitAndCompile(className, str) {
    if (case_insensitive) {
      str = str.toLowerCase();
    }
    str.split(' ').forEach(function(keyword) {
      var pair = keyword.split('|');
      compiled_keywords[pair[0]] = [className, scoreForKeyword(pair[0], pair[1])];
    });
  }
  }

  function scoreForKeyword(keyword, providedScore) {
  // manual scores always win over common keywords
  // so you can force a score of 1 if you really insist
  if (providedScore)
    return Number(providedScore);

  return commonKeyword(keyword) ? 0 : 1;
  }

  function commonKeyword(word) {
  return COMMON_KEYWORDS.includes(word.toLowerCase());
  }

  /*
  Syntax highlighting with language autodetection.
  https://highlightjs.org/
  */

  const escape$1 = escapeHTML;
  const inherit$1 = inherit;
  const { nodeStream: nodeStream$1, mergeStreams: mergeStreams$1 } = utils;


  const HLJS = function(hljs) {
    // Convenience variables for build-in objects
    var ArrayProto = [];

    // Global internal variables used within the highlight.js library.
    var languages = {},
        aliases   = {},
        plugins   = [];

    // safe/production mode - swallows more errors, tries to keep running
    // even if a single syntax or parse hits a fatal error
    var SAFE_MODE = true;

    // Regular expressions used throughout the highlight.js library.
    var fixMarkupRe      = /((^(<[^>]+>|\t|)+|(?:\n)))/gm;

    var spanEndTag = '</span>';
    var LANGUAGE_NOT_FOUND = "Could not find the language '{}', did you forget to load/include a language module?";

    // Global options used when within external APIs. This is modified when
    // calling the `hljs.configure` function.
    var options = {
      noHighlightRe: /^(no-?highlight)$/i,
      languageDetectRe: /\blang(?:uage)?-([\w-]+)\b/i,
      classPrefix: 'hljs-',
      tabReplace: null,
      useBR: false,
      languages: undefined
    };

    /* Utility functions */

    function isNotHighlighted(language) {
      return options.noHighlightRe.test(language);
    }

    function blockLanguage(block) {
      var match;
      var classes = block.className + ' ';

      classes += block.parentNode ? block.parentNode.className : '';

      // language-* takes precedence over non-prefixed class names.
      match = options.languageDetectRe.exec(classes);
      if (match) {
        var language = getLanguage(match[1]);
        if (!language) {
          console.warn(LANGUAGE_NOT_FOUND.replace("{}", match[1]));
          console.warn("Falling back to no-highlight mode for this block.", block);
        }
        return language ? match[1] : 'no-highlight';
      }

      return classes
        .split(/\s+/)
        .find((_class) => isNotHighlighted(_class) || getLanguage(_class))
    }

    /**
     * Core highlighting function.
     *
     * @param {string} languageName - the language to use for highlighting
     * @param {string} code - the code to highlight
     * @param {boolean} ignore_illegals - whether to ignore illegal matches, default is to bail
     * @param {array<mode>} continuation - array of continuation modes
     *
     * @returns an object that represents the result
     * @property {string} language - the language name
     * @property {number} relevance - the relevance score
     * @property {string} value - the highlighted HTML code
     * @property {mode} top - top of the current mode stack
     * @property {boolean} illegal - indicates whether any illegal matches were found
    */
    function highlight(languageName, code, ignore_illegals, continuation) {
      var codeToHighlight = code;

      function endOfMode(mode, lexeme) {
        if (startsWith(mode.endRe, lexeme)) {
          while (mode.endsParent && mode.parent) {
            mode = mode.parent;
          }
          return mode;
        }
        if (mode.endsWithParent) {
          return endOfMode(mode.parent, lexeme);
        }
      }

      function keywordMatch(mode, match) {
        var match_str = language.case_insensitive ? match[0].toLowerCase() : match[0];
        return mode.keywords.hasOwnProperty(match_str) && mode.keywords[match_str];
      }

      function buildSpan(className, insideSpan, leaveOpen, noPrefix) {
        if (!leaveOpen && insideSpan === '') return '';
        if (!className) return insideSpan;

        var classPrefix = noPrefix ? '' : options.classPrefix,
            openSpan    = '<span class="' + classPrefix,
            closeSpan   = leaveOpen ? '' : spanEndTag;

        openSpan += className + '">';

        return openSpan + insideSpan + closeSpan;
      }

      function processKeywords() {
        var keyword_match, last_index, match, result;

        if (!top.keywords)
          return escape$1(mode_buffer);

        result = '';
        last_index = 0;
        top.lexemesRe.lastIndex = 0;
        match = top.lexemesRe.exec(mode_buffer);

        while (match) {
          result += escape$1(mode_buffer.substring(last_index, match.index));
          keyword_match = keywordMatch(top, match);
          if (keyword_match) {
            relevance += keyword_match[1];
            result += buildSpan(keyword_match[0], escape$1(match[0]));
          } else {
            result += escape$1(match[0]);
          }
          last_index = top.lexemesRe.lastIndex;
          match = top.lexemesRe.exec(mode_buffer);
        }
        return result + escape$1(mode_buffer.substr(last_index));
      }

      function processSubLanguage() {
        var explicit = typeof top.subLanguage === 'string';
        if (explicit && !languages[top.subLanguage]) {
          return escape$1(mode_buffer);
        }

        var result = explicit ?
                     highlight(top.subLanguage, mode_buffer, true, continuations[top.subLanguage]) :
                     highlightAuto(mode_buffer, top.subLanguage.length ? top.subLanguage : undefined);

        // Counting embedded language score towards the host language may be disabled
        // with zeroing the containing mode relevance. Use case in point is Markdown that
        // allows XML everywhere and makes every XML snippet to have a much larger Markdown
        // score.
        if (top.relevance > 0) {
          relevance += result.relevance;
        }
        if (explicit) {
          continuations[top.subLanguage] = result.top;
        }
        return buildSpan(result.language, result.value, false, true);
      }

      function processBuffer() {
        result += (top.subLanguage != null ? processSubLanguage() : processKeywords());
        mode_buffer = '';
      }

      function startNewMode(mode) {
        result += mode.className? buildSpan(mode.className, '', true): '';
        top = Object.create(mode, {parent: {value: top}});
      }


      function doBeginMatch(match) {
        var lexeme = match[0];
        var new_mode = match.rule;

        if (new_mode && new_mode.endSameAsBegin) {
          new_mode.endRe = escape( lexeme );
        }

        if (new_mode.skip) {
          mode_buffer += lexeme;
        } else {
          if (new_mode.excludeBegin) {
            mode_buffer += lexeme;
          }
          processBuffer();
          if (!new_mode.returnBegin && !new_mode.excludeBegin) {
            mode_buffer = lexeme;
          }
        }
        startNewMode(new_mode);
        return new_mode.returnBegin ? 0 : lexeme.length;
      }

      function doEndMatch(match) {
        var lexeme = match[0];
        var matchPlusRemainder = codeToHighlight.substr(match.index);
        var end_mode = endOfMode(top, matchPlusRemainder);
        if (!end_mode) { return; }

        var origin = top;
        if (origin.skip) {
          mode_buffer += lexeme;
        } else {
          if (!(origin.returnEnd || origin.excludeEnd)) {
            mode_buffer += lexeme;
          }
          processBuffer();
          if (origin.excludeEnd) {
            mode_buffer = lexeme;
          }
        }
        do {
          if (top.className) {
            result += spanEndTag;
          }
          if (!top.skip && !top.subLanguage) {
            relevance += top.relevance;
          }
          top = top.parent;
        } while (top !== end_mode.parent);
        if (end_mode.starts) {
          if (end_mode.endSameAsBegin) {
            end_mode.starts.endRe = end_mode.endRe;
          }
          startNewMode(end_mode.starts);
        }
        return origin.returnEnd ? 0 : lexeme.length;
      }

      var lastMatch = {};
      function processLexeme(text_before_match, match) {

        var lexeme = match && match[0];

        // add non-matched text to the current mode buffer
        mode_buffer += text_before_match;

        if (lexeme == null) {
          processBuffer();
          return 0;
        }

        // we've found a 0 width match and we're stuck, so we need to advance
        // this happens when we have badly behaved rules that have optional matchers to the degree that
        // sometimes they can end up matching nothing at all
        // Ref: https://github.com/highlightjs/highlight.js/issues/2140
        if (lastMatch.type=="begin" && match.type=="end" && lastMatch.index == match.index && lexeme === "") {
          // spit the "skipped" character that our regex choked on back into the output sequence
          mode_buffer += codeToHighlight.slice(match.index, match.index + 1);
          if (!SAFE_MODE) {
            var err = new Error('0 width match regex');
            err.languageName = languageName;
            err.badRule = lastMatch.rule;
            throw(err);
          }
          return 1;
        }
        lastMatch = match;

        if (match.type==="begin") {
          return doBeginMatch(match);
        } else if (match.type==="illegal" && !ignore_illegals) {
          // illegal match, we do not continue processing
          throw new Error('Illegal lexeme "' + lexeme + '" for mode "' + (top.className || '<unnamed>') + '"');
        } else if (match.type==="end") {
          var processed = doEndMatch(match);
          if (processed != undefined)
            return processed;
        }

        /*
        Why might be find ourselves here?  Only one occasion now.  An end match that was
        triggered but could not be completed.  When might this happen?  When an `endSameasBegin`
        rule sets the end rule to a specific match.  Since the overall mode termination rule that's
        being used to scan the text isn't recompiled that means that any match that LOOKS like
        the end (but is not, because it is not an exact match to the beginning) will
        end up here.  A definite end match, but when `doEndMatch` tries to "reapply"
        the end rule and fails to match, we wind up here, and just silently ignore the end.

        This causes no real harm other than stopping a few times too many.
        */

        mode_buffer += lexeme;
        return lexeme.length;
      }

      var language = getLanguage(languageName);
      if (!language) {
        console.error(LANGUAGE_NOT_FOUND.replace("{}", languageName));
        throw new Error('Unknown language: "' + languageName + '"');
      }

      compileLanguage(language);
      var top = continuation || language;
      var continuations = {}; // keep continuations for sub-languages
      var result = '', current;
      for(current = top; current !== language; current = current.parent) {
        if (current.className) {
          result = buildSpan(current.className, '', true) + result;
        }
      }
      var mode_buffer = '';
      var relevance = 0;
      try {
        var match, count, index = 0;
        while (true) {
          top.terminators.lastIndex = index;
          match = top.terminators.exec(codeToHighlight);
          if (!match)
            break;
          count = processLexeme(codeToHighlight.substring(index, match.index), match);
          index = match.index + count;
        }
        processLexeme(codeToHighlight.substr(index));
        for(current = top; current.parent; current = current.parent) { // close dangling modes
          if (current.className) {
            result += spanEndTag;
          }
        }
        return {
          relevance: relevance,
          value: result,
          illegal:false,
          language: languageName,
          top: top
        };
      } catch (err) {
        if (err.message && err.message.includes('Illegal')) {
          return {
            illegal: true,
            relevance: 0,
            value: escape$1(codeToHighlight)
          };
        } else if (SAFE_MODE) {
          return {
            relevance: 0,
            value: escape$1(codeToHighlight),
            language: languageName,
            top: top,
            errorRaised: err
          };
        } else {
          throw err;
        }
      }
    }

    /*
    Highlighting with language detection. Accepts a string with the code to
    highlight. Returns an object with the following properties:

    - language (detected language)
    - relevance (int)
    - value (an HTML string with highlighting markup)
    - second_best (object with the same structure for second-best heuristically
      detected language, may be absent)

    */
    function highlightAuto(code, languageSubset) {
      languageSubset = languageSubset || options.languages || Object.keys(languages);
      var result = {
        relevance: 0,
        value: escape$1(code)
      };
      var second_best = result;
      languageSubset.filter(getLanguage).filter(autoDetection).forEach(function(name) {
        var current = highlight(name, code, false);
        current.language = name;
        if (current.relevance > second_best.relevance) {
          second_best = current;
        }
        if (current.relevance > result.relevance) {
          second_best = result;
          result = current;
        }
      });
      if (second_best.language) {
        result.second_best = second_best;
      }
      return result;
    }

    /*
    Post-processing of the highlighted markup:

    - replace TABs with something more useful
    - replace real line-breaks with '<br>' for non-pre containers

    */
    function fixMarkup(value) {
      if (!(options.tabReplace || options.useBR)) {
        return value;
      }

      return value.replace(fixMarkupRe, function(match, p1) {
          if (options.useBR && match === '\n') {
            return '<br>';
          } else if (options.tabReplace) {
            return p1.replace(/\t/g, options.tabReplace);
          }
          return '';
      });
    }

    function buildClassName(prevClassName, currentLang, resultLang) {
      var language = currentLang ? aliases[currentLang] : resultLang,
          result   = [prevClassName.trim()];

      if (!prevClassName.match(/\bhljs\b/)) {
        result.push('hljs');
      }

      if (!prevClassName.includes(language)) {
        result.push(language);
      }

      return result.join(' ').trim();
    }

    /*
    Applies highlighting to a DOM node containing code. Accepts a DOM node and
    two optional parameters for fixMarkup.
    */
    function highlightBlock(block) {
      var node, originalStream, result, resultNode, text;
      var language = blockLanguage(block);

      if (isNotHighlighted(language))
          return;

      fire("before:highlightBlock",
        { block: block, language: language});

      if (options.useBR) {
        node = document.createElement('div');
        node.innerHTML = block.innerHTML.replace(/\n/g, '').replace(/<br[ \/]*>/g, '\n');
      } else {
        node = block;
      }
      text = node.textContent;
      result = language ? highlight(language, text, true) : highlightAuto(text);

      originalStream = nodeStream$1(node);
      if (originalStream.length) {
        resultNode = document.createElement('div');
        resultNode.innerHTML = result.value;
        result.value = mergeStreams$1(originalStream, nodeStream$1(resultNode), text);
      }
      result.value = fixMarkup(result.value);

      fire("after:highlightBlock", { block: block, result: result});

      block.innerHTML = result.value;
      block.className = buildClassName(block.className, language, result.language);
      block.result = {
        language: result.language,
        re: result.relevance
      };
      if (result.second_best) {
        block.second_best = {
          language: result.second_best.language,
          re: result.second_best.relevance
        };
      }
    }

    /*
    Updates highlight.js global options with values passed in the form of an object.
    */
    function configure(user_options) {
      options = inherit$1(options, user_options);
    }

    /*
    Applies highlighting to all <pre><code>..</code></pre> blocks on a page.
    */
    function initHighlighting() {
      if (initHighlighting.called)
        return;
      initHighlighting.called = true;

      var blocks = document.querySelectorAll('pre code');
      ArrayProto.forEach.call(blocks, highlightBlock);
    }

    /*
    Attaches highlighting to the page load event.
    */
    function initHighlightingOnLoad() {
      window.addEventListener('DOMContentLoaded', initHighlighting, false);
    }

    var PLAINTEXT_LANGUAGE = { disableAutodetect: true };

    function registerLanguage(name, language) {
      var lang;
      try { lang = language(hljs); }
      catch (error) {
        console.error("Language definition for '{}' could not be registered.".replace("{}", name));
        // hard or soft error
        if (!SAFE_MODE) { throw error; } else { console.error(error); }
        // languages that have serious errors are replaced with essentially a
        // "plaintext" stand-in so that the code blocks will still get normal
        // css classes applied to them - and one bad language won't break the
        // entire highlighter
        lang = PLAINTEXT_LANGUAGE;
      }
      // give it a temporary name if it doesn't have one in the meta-data
      if (!lang.name)
        lang.name = name;
      languages[name] = lang;
      lang.rawDefinition = language.bind(null,hljs);

      if (lang.aliases) {
        lang.aliases.forEach(function(alias) {aliases[alias] = name;});
      }
    }

    function listLanguages() {
      return Object.keys(languages);
    }

    /*
      intended usage: When one language truly requires another

      Unlike `getLanguage`, this will throw when the requested language
      is not available.
    */
    function requireLanguage(name) {
      var lang = getLanguage(name);
      if (lang) { return lang; }

      var err = new Error('The \'{}\' language is required, but not loaded.'.replace('{}',name));
      throw err;
    }

    function getLanguage(name) {
      name = (name || '').toLowerCase();
      return languages[name] || languages[aliases[name]];
    }

    function autoDetection(name) {
      var lang = getLanguage(name);
      return lang && !lang.disableAutodetect;
    }

    function addPlugin(plugin, options) {
      plugins.push(plugin);
    }

    function fire(event, args) {
      var cb = event;
      plugins.forEach(function (plugin) {
        if (plugin[cb]) {
          plugin[cb](args);
        }
      });
    }

    /* Interface definition */

    Object.assign(hljs,{
      highlight,
      highlightAuto,
      fixMarkup,
      highlightBlock,
      configure,
      initHighlighting,
      initHighlightingOnLoad,
      registerLanguage,
      listLanguages,
      getLanguage,
      requireLanguage,
      autoDetection,
      inherit: inherit$1,
      addPlugin
    });

    hljs.debugMode = function() { SAFE_MODE = false; };
    hljs.safeMode = function() { SAFE_MODE = true; };

    for (const key in MODES) {
      if (typeof MODES[key] === "object")
        deepFreeze(MODES[key]);
    }

    // merge all the modes/regexs into our main object
    Object.assign(hljs, MODES);

    return hljs;
  };

  // export an "instance" of the highlighter
  var highlight = HLJS({});

  return highlight;

})));

hljs.registerLanguage('xml', function () {
  'use strict';

  /*
  Language: HTML, XML
  Website: https://www.w3.org/XML/
  Category: common
  */

  function xml(hljs) {
    var XML_IDENT_RE = '[A-Za-z0-9\\._:-]+';
    var XML_ENTITIES = {
      className: 'symbol',
      begin: '&[a-z]+;|&#[0-9]+;|&#x[a-f0-9]+;'
    };
    var XML_META_KEYWORDS = {
  	  begin: '\\s',
  	  contains:[
  	    {
  	      className: 'meta-keyword',
  	      begin: '#?[a-z_][a-z1-9_-]+',
  	      illegal: '\\n',
        }
  	  ]
    };
    var XML_META_PAR_KEYWORDS = hljs.inherit(XML_META_KEYWORDS, {begin: '\\(', end: '\\)'});
    var APOS_META_STRING_MODE = hljs.inherit(hljs.APOS_STRING_MODE, {className: 'meta-string'});
    var QUOTE_META_STRING_MODE = hljs.inherit(hljs.QUOTE_STRING_MODE, {className: 'meta-string'});
    var TAG_INTERNALS = {
      endsWithParent: true,
      illegal: /</,
      relevance: 0,
      contains: [
        {
          className: 'attr',
          begin: XML_IDENT_RE,
          relevance: 0
        },
        {
          begin: /=\s*/,
          relevance: 0,
          contains: [
            {
              className: 'string',
              endsParent: true,
              variants: [
                {begin: /"/, end: /"/, contains: [XML_ENTITIES]},
                {begin: /'/, end: /'/, contains: [XML_ENTITIES]},
                {begin: /[^\s"'=<>`]+/}
              ]
            }
          ]
        }
      ]
    };
    return {
      name: 'HTML, XML',
      aliases: ['html', 'xhtml', 'rss', 'atom', 'xjb', 'xsd', 'xsl', 'plist', 'wsf', 'svg'],
      case_insensitive: true,
      contains: [
        {
          className: 'meta',
          begin: '<![a-z]', end: '>',
          relevance: 10,
          contains: [
  				  XML_META_KEYWORDS,
  				  QUOTE_META_STRING_MODE,
  				  APOS_META_STRING_MODE,
  					XML_META_PAR_KEYWORDS,
  					{
  					  begin: '\\[', end: '\\]',
  					  contains:[
  						  {
  					      className: 'meta',
  					      begin: '<![a-z]', end: '>',
  					      contains: [
  					        XML_META_KEYWORDS,
  					        XML_META_PAR_KEYWORDS,
  					        QUOTE_META_STRING_MODE,
  					        APOS_META_STRING_MODE
  						    ]
  			        }
  					  ]
  				  }
  				]
        },
        hljs.COMMENT(
          '<!--',
          '-->',
          {
            relevance: 10
          }
        ),
        {
          begin: '<\\!\\[CDATA\\[', end: '\\]\\]>',
          relevance: 10
        },
        XML_ENTITIES,
        {
          className: 'meta',
          begin: /<\?xml/, end: /\?>/, relevance: 10
        },
        {
          className: 'tag',
          /*
          The lookahead pattern (?=...) ensures that 'begin' only matches
          '<style' as a single word, followed by a whitespace or an
          ending braket. The '$' is needed for the lexeme to be recognized
          by hljs.subMode() that tests lexemes outside the stream.
          */
          begin: '<style(?=\\s|>)', end: '>',
          keywords: {name: 'style'},
          contains: [TAG_INTERNALS],
          starts: {
            end: '</style>', returnEnd: true,
            subLanguage: ['css', 'xml']
          }
        },
        {
          className: 'tag',
          // See the comment in the <style tag about the lookahead pattern
          begin: '<script(?=\\s|>)', end: '>',
          keywords: {name: 'script'},
          contains: [TAG_INTERNALS],
          starts: {
            end: '\<\/script\>', returnEnd: true,
            subLanguage: ['actionscript', 'javascript', 'handlebars', 'xml']
          }
        },
        {
          className: 'tag',
          begin: '</?', end: '/?>',
          contains: [
            {
              className: 'name', begin: /[^\/><\s]+/, relevance: 0
            },
            TAG_INTERNALS
          ]
        }
      ]
    };
  }

  return xml;
}());

hljs.registerLanguage('json', function () {
  'use strict';

  /*
  Language: JSON
  Description: JSON (JavaScript Object Notation) is a lightweight data-interchange format.
  Author: Ivan Sagalaev <maniac@softwaremaniacs.org>
  Website: http://www.json.org
  Category: common, protocols
  */

  function json(hljs) {
    var LITERALS = {literal: 'true false null'};
    var ALLOWED_COMMENTS = [
      hljs.C_LINE_COMMENT_MODE,
      hljs.C_BLOCK_COMMENT_MODE
    ];
    var TYPES = [
      hljs.QUOTE_STRING_MODE,
      hljs.C_NUMBER_MODE
    ];
    var VALUE_CONTAINER = {
      end: ',', endsWithParent: true, excludeEnd: true,
      contains: TYPES,
      keywords: LITERALS
    };
    var OBJECT = {
      begin: '{', end: '}',
      contains: [
        {
          className: 'attr',
          begin: /"/, end: /"/,
          contains: [hljs.BACKSLASH_ESCAPE],
          illegal: '\\n',
        },
        hljs.inherit(VALUE_CONTAINER, {begin: /:/})
      ].concat(ALLOWED_COMMENTS),
      illegal: '\\S'
    };
    var ARRAY = {
      begin: '\\[', end: '\\]',
      contains: [hljs.inherit(VALUE_CONTAINER)], // inherit is a workaround for a bug that makes shared modes with endsWithParent compile only the ending of one of the parents
      illegal: '\\S'
    };
    TYPES.push(OBJECT, ARRAY);
    ALLOWED_COMMENTS.forEach(function(rule) {
      TYPES.push(rule);
    });
    return {
      name: 'JSON',
      contains: TYPES,
      keywords: LITERALS,
      illegal: '\\S'
    };
  }

  return json;
}());

hljs.registerLanguage('keypath', function () {
    'use strict';

    /*
    Language: keypath
    Category: common, config
    */

    function keypath(hljs) {
        return {
            name: 'keypath',
            case_insensitive: true,
            contains: [{
                className: 'attr',
                begin: /^/,
                end: / /,
                contains: [{
                    className: 'string',
                    begin: /\{/,
                    end: /\}/,
                }]
            }]
        };
    }

    return keypath;
}());

hljs.registerLanguage('xpath', function () {
    'use strict';

    /*
    Language: xpath
    Category: common, config
    */

    function xpath(hljs) {
        return {
            name: 'xpath',
            case_insensitive: true,
            contains: [{
                className: 'attr',
                begin: /^/,
                end: / /,
                contains: [{
                    className: 'string',
                    begin: /\[/,
                    end: /\]/,
                }]
            }]
        };
    }

    return xpath;
}());

hljs.registerLanguage('set', function () {
  'use strict';

  /*
  Language: .properties
  Contributors: Valentin Aitken <valentin@nalisbg.com>, Egor Rogov <e.rogov@postgrespro.ru>
  Website: https://en.wikipedia.org/wiki/.properties
  Category: common, config
  */

  function set(hljs) {

    // whitespaces: space, tab, formfeed
    var WS0 = '[ \\t\\f]*';
    var WS1 = '[ \\t\\f]+';
    // delimiter
    var DELIM = '(' + WS0+'[:=]'+WS0+ '|' + WS1 + ')';
    var KEY_ALPHANUM = '([^\\\\\\W:= \\t\\f\\n]|\\\\.)+';
    var KEY_OTHER = '([^\\\\:= \\t\\f\\n]|\\\\.)+';

    var DELIM_AND_VALUE = {
            // skip DELIM
            end: DELIM,
            relevance: 0,
            starts: {
              // value: everything until end of line (again, taking into account backslashes)
              className: 'string',
              end: /$/,
              relevance: 0,
              contains: [
                { begin: '\\\\\\n' }
              ]
            }
          };

    return {
      name: '.properties',
      case_insensitive: true,
      illegal: /\S/,
      contains: [
        hljs.COMMENT('^\\s*[!#]', '$'),
        // key: everything until whitespace or = or : (taking into account backslashes)
        // case of a "normal" key
        {
          begin: KEY_ALPHANUM + DELIM,
          returnBegin: true,
          contains: [
            {
              className: 'attr',
              begin: KEY_ALPHANUM,
              endsParent: true,
              relevance: 0
            }
          ],
          starts: DELIM_AND_VALUE
        },
        // case of key containing non-alphanumeric chars => relevance = 0
        {
          begin: KEY_OTHER + DELIM,
          returnBegin: true,
          relevance: 0,
          contains: [
            {
              className: 'meta',
              begin: KEY_OTHER,
              endsParent: true,
              relevance: 0
            }
          ],
          starts: DELIM_AND_VALUE
        },
        // case of an empty key
        {
          className: 'attr',
          relevance: 0,
          begin: KEY_OTHER + WS0 + '$'
        }
      ]
    };
  }

  return set;
}());
