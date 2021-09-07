// Copied from https://github.com/ueberdosis/tiptap/blob/main/packages/extension-code-block/src/code-block.ts
// and I changed the regexes to not require a space at the end. We lose the ability to set code language,
// but it's ok.

import { Node } from 'https://cdn.skypack.dev/pin/@tiptap/core@v2.0.0-beta.103-1EijC0NTac4wtSFPah5m/mode=imports,min/optimized/@tiptap/core.js';
import { textblockTypeInputRule } from 'https://cdn.skypack.dev/pin/prosemirror-inputrules@v1.1.3-H1ealaYJwb2N1q4DVZgX/mode=imports,min/optimized/prosemirror-inputrules.js';

export const backtickInputRegex = /^```$/
export const tildeInputRegex = /^~~~$/

export const CodeBlock = Node.create({
  name: 'codeBlock',

  defaultOptions: {
    languageClassPrefix: 'language-',
    HTMLAttributes: {},
  },

  content: 'text*',

  marks: '',

  group: 'block',

  code: true,

  defining: true,

  addAttributes() {
    return {
      language: {
        default: null,
        parseHTML: element => {
          const classAttribute = element.firstElementChild?.getAttribute('class')

          if (!classAttribute) {
            return null
          }

          const regexLanguageClassPrefix = new RegExp(`^(${this.options.languageClassPrefix})`)

          return {
            language: classAttribute.replace(regexLanguageClassPrefix, ''),
          }
        },
        renderHTML: attributes => {
          if (!attributes.language) {
            return null
          }

          return {
            class: this.options.languageClassPrefix + attributes.language,
          }
        },
      },
    }
  },

  parseHTML() {
    return [
      {
        tag: 'pre',
        preserveWhitespace: 'full',
      },
    ]
  },

  renderHTML({ HTMLAttributes }) {
    return ['pre', this.options.HTMLAttributes, ['code', HTMLAttributes, 0]]
  },

  addCommands() {
    return {
      setCodeBlock: attributes => ({ commands }) => {
        return commands.setNode('codeBlock', attributes)
      },
      toggleCodeBlock: attributes => ({ commands }) => {
        return commands.toggleNode('codeBlock', 'paragraph', attributes)
      },
    }
  },

  addKeyboardShortcuts() {
    return {
      'Mod-Alt-c': () => this.editor.commands.toggleCodeBlock(),

      // remove code block when at start of document or code block is empty
      Backspace: () => {
        const { empty, $anchor } = this.editor.state.selection
        const isAtStart = $anchor.pos === 1

        if (!empty || $anchor.parent.type.name !== this.name) {
          return false
        }

        if (isAtStart || !$anchor.parent.textContent.length) {
          return this.editor.commands.clearNodes()
        }

        return false
      },
    }
  },

  addInputRules() {
    return [
      textblockTypeInputRule(backtickInputRegex, this.type, ({ groups }) => groups),
      textblockTypeInputRule(tildeInputRegex, this.type, ({ groups }) => groups),
    ]
  },
})

export default CodeBlock;