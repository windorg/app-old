import { Mark, markPasteRule, mergeAttributes } from 'https://cdn.skypack.dev/pin/@tiptap/core@v2.0.0-beta.103-1EijC0NTac4wtSFPah5m/mode=imports,min/optimized/@tiptap/core.js';
import { Plugin, PluginKey } from 'https://cdn.skypack.dev/pin/prosemirror-state@v1.3.4-3srM35WPWsr1uSWCfm5j/mode=imports,min/optimized/prosemirror-state.js';

/**
 * A regex that matches any string that contains a link
 */
export const pasteRegex = /https?:\/\/(?:www\.)?[-a-zA-Z0-9@:%._+~#=]{1,256}\.[a-zA-Z]{2,}\b(?:[-a-zA-Z0-9@:%._+~#=?!&/]*)(?:[-a-zA-Z0-9@:%._+~#=?!&/]*)/gi

/**
 * A regex that matches an url
 */
export const pasteRegexExact = /^https?:\/\/(?:www\.)?[-a-zA-Z0-9@:%._+~#=]{1,256}\.[a-zA-Z]{2,}\b(?:[-a-zA-Z0-9@:%._+~#=?!&/]*)(?:[-a-zA-Z0-9@:%._+~#=?!&/]*)$/gi

export const Link = Mark.create({
  name: 'link',

  priority: 1000,

  inclusive: false,

  defaultOptions: {
    linkOnPaste: true,
    HTMLAttributes: {
      target: '_blank',
      rel: 'noopener noreferrer nofollow',
    },
  },

  addAttributes() {
    return {
      href: {
        default: null,
      },
      target: {
        default: this.options.HTMLAttributes.target,
      },
    }
  },

  parseHTML() {
    return [
      { tag: 'a[href]' },
    ]
  },

  renderHTML({ HTMLAttributes }) {
    return ['a', mergeAttributes(this.options.HTMLAttributes, HTMLAttributes), 0]
  },

  addCommands() {
    return {
      setLink: attributes => ({ commands }) => {
        return commands.setMark('link', attributes)
      },
      toggleLink: attributes => ({ commands }) => {
        return commands.toggleMark('link', attributes, { extendEmptyMarkRange: true })
      },
      unsetLink: () => ({ commands }) => {
        return commands.unsetMark('link', { extendEmptyMarkRange: true })
      },
      showLinkDialog: () => ({ commands }) => {
        bootbox.prompt({
            title: 'Set link', 
            backdrop: true,
            callback: (result) => {
                if (result) {
                    this.editor.commands.setLink({href: result})
                }
            }
        })
      }
    }
  },

  addPasteRules() {
    return [
      markPasteRule(pasteRegex, this.type, match => ({ href: match[0] })),
    ]
  },

  addKeyboardShortcuts() {
      return {
          'Mod-k': () => this.editor.commands.showLinkDialog(),
      }
  },

  addProseMirrorPlugins() {
    const plugins = []

    plugins.push(
    new Plugin({
        key: new PluginKey('handleClickLink'),
        props: {
        handleClick: (view, pos, event) => {
            const attrs = this.editor.getAttributes('link')
            const link = event.target?.closest('a')

            if (link && attrs.href) {
                bootbox.prompt({
                    title: 'Change link', 
                    value: attrs.href,
                    placeholder: 'Empty = erases the link',
                    backdrop: true,
                    callback: (result) => {
                        if (result) {
                            this.editor.chain().extendMarkRange('link').updateAttributes('link', {href: result}).run()
                        } else if (result === '') {
                            this.editor.commands.unsetLink()
                        }
                    }
                })
                return true
            }

            return false
        },
        },
    }),
    )

    if (this.options.linkOnPaste) {
      plugins.push(
        new Plugin({
          key: new PluginKey('handlePasteLink'),
          props: {
            handlePaste: (view, event, slice) => {
              const { state } = view
              const { selection } = state
              const { empty } = selection

              if (empty) {
                return false
              }

              let textContent = ''

              slice.content.forEach(node => {
                textContent += node.textContent
              })

              if (!textContent || !textContent.match(pasteRegexExact)) {
                return false
              }

              this.editor.commands.setMark(this.type, {
                href: textContent,
              })

              return true
            },
          },
        }),
      )
    }

    return plugins
  },
})

export default Link