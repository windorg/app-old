import {
  Mark,
  markInputRule,
  markPasteRule,
  mergeAttributes,
} from 'https://cdn.skypack.dev/pin/@tiptap/core@v2.0.0-beta.103-1EijC0NTac4wtSFPah5m/mode=imports,min/optimized/@tiptap/core.js';

export const inputRegex = /(?:^|\s)((?:`)((?:[^`]+))(?:`))$/gm
export const pasteRegex = /(?:^|\s)((?:`)((?:[^`]+))(?:`))/gm

export const Code = Mark.create({
  name: 'code',

  defaultOptions: {
    HTMLAttributes: {},
  },

  excludes: '_',

  parseHTML() {
    return [
      { tag: 'code' },
    ]
  },

  renderHTML({ HTMLAttributes }) {
    return ['code', mergeAttributes(this.options.HTMLAttributes, HTMLAttributes), 0]
  },

  addCommands() {
    return {
      setCode: () => ({ commands }) => {
        return commands.setMark('code')
      },
      toggleCode: () => ({ commands }) => {
        return commands.toggleMark('code')
      },
      unsetCode: () => ({ commands }) => {
        return commands.unsetMark('code')
      },
    }
  },

  addKeyboardShortcuts() {
    return {
      'Mod-e': () => this.editor.commands.toggleCode(),
      // Taken from https://github.com/ueberdosis/tiptap/pull/1200/files
      ArrowRight: () => {
        const state = this.editor.state
        const { from, to } = state.selection
      
        if (from > 1 && from === to) {
          let codeOnLeft = false
          state.doc.nodesBetween(from - 1, to - 1, node => {
            const code = node.marks.find(markItem => markItem.type.name === 'code')
            if (code) codeOnLeft = true
          })
      
          let noCodeUnderCursor = true
          state.doc.nodesBetween(from, to, node => {
            const code = node.marks.find(markItem => markItem.type.name === 'code')
            if (code) noCodeUnderCursor = false
          })
      
          let nothingOnRight = true
          state.doc.nodesBetween(from + 1, to + 1, node => {
            if (node) nothingOnRight = false
          })
      
          if (codeOnLeft && noCodeUnderCursor && nothingOnRight) {
            return this.editor
              .chain()
              .unsetCode()
              .insertContent([{ type: 'text', text: ' ' }])
              .run()
          }
        }
      
        return false
      },
    }
  },

  addInputRules() {
    return [
      markInputRule(inputRegex, this.type),
    ]
  },

  addPasteRules() {
    return [
      markPasteRule(pasteRegex, this.type),
    ]
  },
})