// Removed the Ctrl-Enter shortcut

import { Node, mergeAttributes } from 'https://cdn.skypack.dev/pin/@tiptap/core@v2.0.0-beta.103-1EijC0NTac4wtSFPah5m/mode=imports,min/optimized/@tiptap/core.js';

export const HardBreak = Node.create({
  name: 'hardBreak',

  defaultOptions: {
    HTMLAttributes: {},
  },

  inline: true,

  group: 'inline',

  selectable: false,

  parseHTML() {
    return [
      { tag: 'br' },
    ]
  },

  renderHTML({ HTMLAttributes }) {
    return ['br', mergeAttributes(this.options.HTMLAttributes, HTMLAttributes)]
  },

  renderText() {
    return '\n'
  },

  addCommands() {
    return {
      setHardBreak: () => ({ commands }) => {
        return commands.first([
          () => commands.exitCode(),
          () => commands.insertContent({ type: this.name }),
        ])
      },
    }
  },

  addKeyboardShortcuts() {
    return {
      // Just removing the shortcut doesn't work!
      'Mod-Enter': () => this.editor.commands.insertContent(''),
      'Shift-Enter': () => this.editor.commands.setHardBreak(),
    }
  },
})

export default HardBreak;