;;; ../dotfiles/home/.doom.d/+xwwp.el -*- lexical-binding: t; -*-

(use-package! xwwp)

(defun xwidget-webkit-search-forward (text)
  "Search forward of `text'"
  (interactive "sSearch: " xwidget-webkit-mode)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   (format "window.find(\"%s\");" text)))

(defun xwidget-webkit-test ()
  "Search forward of `text'"
  (interactive)
  (xwidget-webkit-execute-script
   (xwidget-webkit-current-session)
   "
(() => {
  const alphabet = 'abcdefghijklmnopqrstuvwxyz'.split('')
  const currentLinkItems = []

  function getNextKeyCombination(index) {
    let halfIndex = Math.floor(alphabet.length / 2);
    if (index < halfIndex) {
      return alphabet[index];
    } else {
      index -= halfIndex;
      return alphabet[Math.floor(index / alphabet.length) + halfIndex] + alphabet[index % alphabet.length];
    }
  }

  function createLinkItem (link, rect, key) {
    var item = document.createElement('span')
    item.setAttribute('style', 'position: absolute; padding: 1px 3px 0px 3px; background-color: yellow; color: black; z-index: 9999; font-family: Helvetica, Arial, sans-serif;font-weight: bold;font-size: 12px; background: linear-gradient(to bottom, #FFF785 0%,#FFC542 100%); border: solid 1px #C38A22; border-radius: 3px; box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3);')

    item.textContent = key

    item.style.top = (window.scrollY + rect.top) + 'px'
    item.style.left = (window.scrollX + rect.left) + 'px'

    return item
  }

  function isVisible (rect) {
    return (
      rect.top > 0 &&
        rect.top < window.innerHeight &&
        rect.left > 0 &&
        rect.left < window.innerWidth
    )
  }

  function showLinkKeys() {
    const links = [];
    const linkRects = [];

    [].slice.call(document.querySelectorAll('a, button, input, textarea, select')).forEach(function (link) {
      var rect = link.getBoundingClientRect()
      if (isVisible(rect)) {
        links.push(link)
        linkRects.push(rect)
      }
    })

    links.forEach(function (link, i) {
      var key = getNextKeyCombination(currentLinkItems.length)
      var item = createLinkItem(link, linkRects[i], key)
      currentLinkItems.push({
        link: link,
        element: item,
        key: key
      })
      document.body.appendChild(item)
    })
  }
  showLinkKeys();
  return 'hello world';
"
   (lambda (arg) (message "hello"))))

(xwwp-js-def follow-link jd-test ()
  "Test.""
  var jd = {};
  jd[1] = 'test';
  return jd;
")

;; (97) (32 . id('hotsearch-content-wrapper')/LI[2]/A[1])
(defun my-overlay-fn (path leaf)
  "My overlay fn."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (apply #'string (reverse path)))
         (xpath (cdr leaf)))
    (xwwp-follow-link-highlight-candidates (xwidget-webkit-current-session) str xpath))
  )
(xwwp-js-def follow-link highlight-candidates (str xpath)
  "Highlight candidates.""
function lookupElementByXPath(path) {
  var evaluator = new XPathEvaluator();
  var result = evaluator.evaluate(path, document.documentElement, null,XPathResult.FIRST_ORDERED_NODE_TYPE, null);
  return  result.singleNodeValue;
}
function createLinkItem (link, rect, key) {
  var item = document.createElement('span')
  item.setAttribute('style', 'position: absolute; padding: 1px 3px 0px 3px; background-color: yellow; color: black; z-index: 9999; font-family: Helvetica, Arial, sans-serif;font-weight: bold;font-size: 12px; background: linear-gradient(to bottom, #FFF785 0%,#FFC542 100%); border: solid 1px #C38A22; border-radius: 3px; box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3);')

  item.textContent = key

  item.style.top = (window.scrollY + rect.top) + 'px'
  item.style.left = (window.scrollX + rect.left) + 'px'

  return item
}
var link = lookupElementByXPath(String(xpath).toLowerCase());
var rect = link.getBoundingClientRect();
var item = createLinkItem(link, rect, str);
window.__xwidget_plus_follow_link_items.push(item)
document.body.appendChild(item);
")

(xwwp-js-def follow-link clear-items ()
  "Cleanup the items.""
(window.__xwidget_plus_follow_link_items || []).forEach(item => {
   item.remove();
});
")

(defun my-cleanup-fn ()
  (interactive)
  (xwwp-follow-link-clear-items (xwidget-webkit-current-session)))

(defun jd-test-callback (candidates)
  "Jd-test callback."
  (let ((res (unwind-protect
                 (avy-read (avy-tree (append candidates nil) avy-keys) #'my-overlay-fn #'my-cleanup-fn))))
    (cond
     ((null res)
      (message "zero candidates"))
     (t
      (funcall avy-pre-action res)
      (setq res (car res))
      (funcall 'avy-action-goto
               (if (consp res)
                   (car res)
                 res))
      res))))

(defun xwidget-webkit-jd-try ()
  "My try."
  (interactive)
  (xwwp-follow-link-jd-test (xwidget-webkit-current-session) #'jd-test-callback))


(xwwp-js-def follow-link get-candidate ()
  "Fetch all visible, non empty links from current page.""
var r = {};
window.__xwidget_plus_follow_link_candidates = Array.from(document.querySelectorAll('a'));
window.__xwidget_plus_follow_link_candidates.forEach((a, i) => {
    if (a.offsetWidth || a.offsetHeight || a.getClientRects().length) {
        if (a.innerText.match(/\\\\S/))
            r[i] = [a.innerText, a.href];
    }
});
return r;
")


(xwwp-js-def follow-link fetch-linkr ()
  "Fetch all visible, non empty links from the current page.""
window.__xwidget_plus_follow_link_items = [];
var r = {};
window.__xwidget_plus_follow_link_candidates = Array.from(document.querySelectorAll('a'));
function getPathTo(element) {
  if (element.id!=='')
    return `id('${element.id}')`;
  if (element===document.body)
    return element.tagName;

  var ix= 0;
  var siblings= element.parentNode.childNodes;
  for (var i= 0; i<siblings.length; i++) {
    var sibling= siblings[i];
    if (sibling===element)
      return getPathTo(element.parentNode)+'/'+element.tagName+'['+(ix+1)+']';
    if (sibling.nodeType===1 && sibling.tagName===element.tagName)
      ix++;
  }
}
window.__xwidget_plus_follow_link_candidates.forEach((a, i) => {
    if (a.offsetWidth || a.offsetHeight || a.getClientRects().length) {
        r[i] = getPathTo(a);
    }
});
return r;
")

(defun xwidget-webkit--get-candidates ()
  "Test."
  (interactive)
  (xwwp-js-inject (xwidget-webkit-current-session) 'follow-link)
  (xwwp-follow-link-fetch-linkr (xwidget-webkit-current-session) #'jd-test-callback))
