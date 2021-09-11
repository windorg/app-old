// skypack pinned URLs can be obtained by visiting links like 'https://cdn.skypack.dev/@tiptap/core'
import { Editor, Extension } from 'https://cdn.skypack.dev/pin/@tiptap/core@v2.0.0-beta.103-1EijC0NTac4wtSFPah5m/mode=imports,min/optimized/@tiptap/core.js';
import StarterKit from 'https://cdn.skypack.dev/pin/@tiptap/starter-kit@v2.0.0-beta.102-HbN0d7mePZcMK0Js0Mvg/mode=imports,min/optimized/@tiptap/starter-kit.js';
import Typography from 'https://cdn.skypack.dev/pin/@tiptap/extension-typography@v2.0.0-beta.14-DZDA2cSqeTwyIh1QoLts/mode=imports,min/optimized/@tiptap/extension-typography.js';
import HardBreak from './tiptap-hard-break.js';
import CodeBlock from './tiptap-code-block.js';
import { TrailingNode } from './tiptap-trailing-node.js';
import Link from './tiptap-link.js';

import TurndownService from 'https://cdn.skypack.dev/pin/turndown@v7.1.1-5XhlHlbwN5u9Cwiz1WQT/mode=imports,min/optimized/turndown.js';
import * as commonmark from 'https://cdn.skypack.dev/pin/commonmark@v0.30.0-RYoFWHhKKHdGwKqr6TYw/mode=imports,min/optimized/commonmark.js';

var turndownService = new TurndownService();
var cmarkReader = new commonmark.Parser();
var cmarkWriter = new commonmark.HtmlRenderer({safe: true});

// Still doesn't work properly with turbolinks though
class AutoSize extends HTMLTextAreaElement {
    constructor () {
        super();
        autosize(this);
        $(this).on('focus', function () {
            autosize.update(this);
        });
    }
    connectedCallback() {
        autosize.update(this);
    }
}
customElements.define(
    "auto-size",
    AutoSize,
    { extends: "textarea" }
);

const SubmitShortcut = Extension.create({
    name: 'SubmitShortcut',
    addKeyboardShortcuts() {
        return {
            'Mod-Enter': () => window.submitForm($(this.editor.options.element).closest('form')[0])
        }
    }
})

// From https://stackoverflow.com/a/53744331/615030
function loadScript(scriptUrl) {
  const script = document.createElement('script');
  script.src = scriptUrl;
  document.body.appendChild(script);
  return new Promise((res, rej) => {
    script.onload = function() {
      res();
    }
    script.onerror = function () {
      rej();
    }
  });
}

const headwayConfig = {
    selector: "#changelog-badge",
    trigger: "#changelog-trigger",
    account: "xYvgB7"
};

// Things that will only be called once.
//
// Note that data-turbolinks-permanent doesn't always work (e.g. it doesn't work on the Headway widget)
function onReady() {
    // Headway (changelog)
    loadScript('https://cdn.headwayapp.co/widget.js')
      .then(() => {Headway.init(headwayConfig)})
      .catch(() => {});
}

// Things that will be called on load, or Turbolinks reloads
function onReadyOrTurbo() {
    // Headway (changelog)
    if (typeof Headway !== 'undefined') {Headway.init(headwayConfig)};

    // Autosize
    autosize(document.querySelectorAll("textarea[is='auto-size']"));

    const newTiptap = function (content) {
        return new Editor({
            extensions: [
                StarterKit,
                HardBreak,
                CodeBlock,
                Typography,
                Link,
                TrailingNode,
                SubmitShortcut,
            ],
            editorProps: {
                attributes: {
                class: 'form-control',
                },
            },
            content: content,
        });
    }

    // Revitalize tiptap editors that were killed by a turbolinks forth/back visit
    $('textarea.use-tiptap.tiptap-processed').each(function(_, el) {
        const editorElement = $(el).next().children()[0];
        if (!editorElement.editor) {
            const editor = newTiptap(editorElement.innerHTML);
            $(editorElement).replaceWith(editor.options.element);
        }
    });
    // Create tiptap editors for new textareas
    $('textarea.use-tiptap:not(.tiptap-processed)').each(function(_, el) {
        const editor = newTiptap(cmarkWriter.render(cmarkReader.parse(el.value)));
        $(el).after(editor.options.element);
        $(el).addClass('tiptap-processed');
        $(el).hide();
    });
};

$(document).ready(function () {
    onReady();
    onReadyOrTurbo();
});
$(document).on('turbolinks:load', function () {
    onReadyOrTurbo();
});

// Register a cunning form submit handler that will turn HTML from rich editors into Markdown before submitting the
// form. Note that IHP doesn't use the submit method and instead uses its own logic for submitting the form.
//
// Depends on editors being preceded by plain textareas that we mirror the content to.
//
// https://github.com/digitallyinduced/ihp/blob/90d16d52eb05f2b8086ab3336035669caafb67de/lib/IHP/static/helpers.js#L163
window.__ihp_submitForm = window.submitForm;
window.submitForm = function(form, possibleClickedButton) {
    $(form).find('.ProseMirror').each(function(_, editorElement) {
        $(editorElement).parent().prev('textarea')[0].value = turndownService.turndown(editorElement.editor.getHTML());
    });
    return window.__ihp_submitForm(form, possibleClickedButton);
};