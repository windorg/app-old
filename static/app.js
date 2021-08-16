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

// This is called when the page is loaded or changed by turbolinks
$(document).on('ready turbolinks:load', function () {
    autosize(document.querySelectorAll("textarea[is='auto-size']"));
});
