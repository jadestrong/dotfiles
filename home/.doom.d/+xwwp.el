;;; ../dotfiles/home/.doom.d/+xwwp.el -*- lexical-binding: t; -*-

(use-package! xwwp)

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

(xwwp-js-def follow-link highlight-candidates (str xpath)
  "Highlight candidates.""
function lookupElementByXPath(path) {
  var evaluator = new XPathEvaluator();
  var result = evaluator.evaluate(path, document.documentElement, null, XPathResult.FIRST_ORDERED_NODE_TYPE, null);
  return result.singleNodeValue;
}
function createLinkItem (link, rect, key) {
  var item = document.createElement('span')
  item.setAttribute('style', 'position: absolute; padding: 1px 3px 0px 3px; background-color: yellow; color: black; z-index: 9999; font-family: Helvetica, Arial, sans-serif;font-weight: bold;font-size: 12px; background: linear-gradient(to bottom, #FFF785 0%,#FFC542 100%); border: solid 1px #C38A22; border-radius: 3px; box-shadow: 0px 3px 7px 0px rgba(0, 0, 0, 0.3);')
  item.textContent = key
  item.style.top = (window.scrollY + rect.top) + 'px'
  item.style.left = (window.scrollX + rect.left) + 'px'
  return item
}
console.log(xpath);
var link = lookupElementByXPath(xpath);
console.log(link);
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

(xwwp-js-def follow-link goto-candidate (res)
  "Goto candidate""
console.log(res);
function lookupElementByXPath(path) {
  var evaluator = new XPathEvaluator();
  var result = evaluator.evaluate(path, document.documentElement, null,XPathResult.FIRST_ORDERED_NODE_TYPE, null);
  return result.singleNodeValue;
}
const link = lookupElementByXPath(res);
const tag = link.tagName;
console.log(tag);
if (tag === 'A') {
  window.open(link.href);
} else {
  console.log(link);
}
")

(defun my-goto (res)
  "Goto candidate."
  (message "res  %S" res)
  (xwwp-follow-link-goto-candidate (xwidget-webkit-current-session) res))

;; (97) (32 . id('hotsearch-content-wrapper')/LI[2]/A[1])
(defun my-overlay-fn (path leaf)
  "My overlay fn."
  (let* ((path (mapcar #'avy--key-to-char path))
         (str (apply #'string (reverse path)))
         (xpath (cdr leaf)))
    (xwwp-follow-link-highlight-candidates (xwidget-webkit-current-session) str xpath))
  )

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
      ;; (funcall avy-pre-action res)
      (message "--- %s" res)
      (setq res (cdr res))
      (funcall 'my-goto
               (if (consp res)
                   (car res)
                 res))
      res))))

(defun xwidget-webkit--get-candidates ()
  "Test."
  (interactive)
  (xwwp-js-inject (xwidget-webkit-current-session) 'follow-link)
  (xwwp-follow-link-fetch-linkr (xwidget-webkit-current-session) #'jd-test-callback))
