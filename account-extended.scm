;; -*-scheme-*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, contact:
;;
;; Free Software Foundation           Voice:  +1-617-542-5942
;; 51 Franklin Street, Fifth Floor    Fax:    +1-617-542-2652
;; Boston, MA  02110-1301,  USA       gnu@gnu.org
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; This is a sample guile report generator for GnuCash.
;; It illustrates the basic techniques used to create
;; new reports for GnuCash.

(define-module (gnucash reports example account-extended))

(use-modules (gnucash engine))
(use-modules (gnucash utilities)) 
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (gnucash html))

(debug-enable 'backtrace)

(define (balanceincdesc acct date)
  (+ 
    (apply + 
      (map
	(lambda(acctdescendants)
	  (* (xaccAccountGetBalanceAsOfDate acctdescendants date) -1))
	    (gnc-account-get-descendants acct)))
    (* (xaccAccountGetBalanceAsOfDate acct date) -1)))

;; this recursive function sums the balances of sub-accounts into a parent acoount.
;; it inverts the sign of the result. A single number is returned. 
(define (parent-check acct searchstr)	    
  (if (string-contains (gnc-account-get-full-name acct) searchstr)
    #t
    (if (not (gnc-account-is-root acct))
      (parent-check (gnc-account-get-parent acct) searchstr)
      #f)))

;;this takes a list of accounts and returns a filtered list with only accounts
;;which contain Income in the name or have a parent that contains Income 
(define (account-name-filter lst searchstr)
  (if (null? lst) 
    '()
    (if (parent-check (car lst) searchstr) 
      (cons (car lst) (account-name-filter (cdr lst) searchstr))
      (account-name-filter (cdr lst) searchstr))))

;;this works through each of the originally selected accounts and gets all their
;;descendants to look for accounts containing the search string in the name
(define (filtered-accounts lst sublst searchstr)
  (if (null? lst)
      sublst 
      (filtered-accounts 
        (cdr lst) 
        (append sublst (account-name-filter (gnc-account-get-descendants (car lst)) searchstr))
        searchstr)))

(define (remove-duplicates lst removed)
  (if (null? lst)
      removed
      (if 
        (= 
          (length 
            (filter 
              (lambda(x) (not (equal? (car lst) x))) 
                (cdr lst))) 
          (length 
              (cdr lst)))    
        (remove-duplicates (cdr lst) (cons (car lst) removed)) 
        (remove-duplicates (cdr lst) removed))))



;; This function will generate a set of options that GnuCash
;; will use to display a dialog where the user can select
;; values for your report's parameters.
(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         ;; This is just a helper function for making options.
         ;; See libgnucash/app-utils/options.scm for details.
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))
    
    ;; This is a date/time option. The user can pick a date and,
    ;; possibly, a time. Times are stored as an integer specifying
    ;; number of seconds measured from Jan 1, 1970, i.e.,
    ;; Unix time. The last option is false, so the user can only
    ;; select a date, not a time. The default value is the current
    ;; time.
    (add-option
     (gnc:make-date-option
      (N_ "Options") (N_ "Start Date")
      "d" (N_ "This is a date option.")
      (lambda () (cons 'absolute (current-time)))
      #f 'absolute #f ))
    
    ;; This is another date option, but the user can also select
    ;; the time.
    (add-option
     (gnc:make-date-option
      (N_ "Options") (N_ "End Date")
      "e" (N_ "This is a date option.")
      (lambda () (cons 'absolute (current-time)))
      #f 'absolute #f ))
    
    (add-option
     (gnc:make-date-option
      (N_ "Options") (N_ "Combo Date Option")
      "y" (N_ "This is a combination date option.")
      (lambda () (cons 'relative 'start-cal-year))
      #f 'both '(start-cal-year start-prev-year end-prev-year) ))
    
    (add-option
     (gnc:make-date-option
      (N_ "Options") (N_ "Relative Date Option")
      "x" (N_ "This is a relative date option.")
      (lambda () (cons 'relative 'start-cal-year))
      #f 'relative '(start-cal-year start-prev-year end-prev-year) ))
    
    ;; This is a color option, defined by rgba values. A color value
    ;; is a list where the elements are the red, green, blue, and
    ;; alpha channel values respectively. The penultimate argument
    ;; (255) is the allowed range of rgba values. The final argument
    ;; (#f) indicates the alpha value should be ignored. You can get
    ;; a color string from a color option with gnc:color-option->html,
    ;; which will scale the values appropriately according the range.
    (add-option
     (gnc:make-color-option
      (N_ "Options") (N_ "Background Color")
      "f" (N_ "This is a color option.")
      (list #xf6 #xff #xdb #xff)
      255
      #f))
    
    ;; This is an account list option. The user can select one
    ;; or (possibly) more accounts from the list of accounts
    ;; in the current file. Values are scheme handles to actual
    ;; C pointers to accounts. 
    ;; The #f value indicates that any account will be accepted.
    ;; Instead of a #f values, you could provide a function that
    ;; accepts a list of account values and returns a pair. If
    ;; the first element is #t, the second element is the list
    ;; of accounts actually accepted. If the first element is
    ;; #f, the accounts are rejected and the second element is
    ;; and error string. The last argument is #t which means
    ;; the user is allowed to select more than one account.
    ;; The default value for this option is the currently
    ;; selected account in the main window, if any.
    (add-option
     (gnc:make-account-list-option
      (N_ "Accounts") (N_ "An account list option")
      "g" (N_ "This is an account list option.")
      ;; FIXME : this used to be gnc:get-current-accounts, but 
      ;; that doesn't exist any more.
      (lambda () '())
      #f #t))
    
        (gnc:options-set-default-section options "Options")      
    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (account-extended-renderer report-obj)
  ;; These are some helper functions for looking up option values.
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  ;; The first thing we do is make local variables for all the specific
  ;; options in the set of options given to the function. This set will
  ;; be generated by the options generator above.
  (let ((start-date     (gnc:date-option-absolute-time
                       (op-value "Options" "Start Date")))
        (end-date    (gnc:date-option-absolute-time
                       (op-value "Options" "End Date")))
        (rel-date-val (gnc:date-option-absolute-time
                       (op-value "Options" "Relative Date Option")))
        (combo-date-val (gnc:date-option-absolute-time
                         (op-value "Options" "Combo Date Option")))
        (bg-color-op  (get-op   "Options" "Background Color"))
        (accounts     (op-value "Accounts"   "An account list option")))
       

    ;; these are samples of different date options. for a simple
    ;; date with day, month, and year but no time you should use
    ;; qof-print-date
    (let (
          (start-date-string (gnc-print-time64 start-date "%x"))
          (end-date-string (gnc-print-time64 end-date "%x"))
          (rel-date-string (gnc-print-time64 rel-date-val "%x"))
          (combo-date-string (gnc-print-time64 combo-date-val "%x"))
        ;; document will be the HTML document that we return.
        (document (gnc:make-html-document)))

        (define (accounts-total splits total acct)
          (if (null? splits)
            total
            (if (and (> (xaccTransGetDate (xaccSplitGetParent (car splits))) start-date) (< (xaccTransGetDate (xaccSplitGetParent (car splits))) end-date))
              (if (string-contains (gnc-account-get-full-name (xaccSplitGetAccount (car splits))) (gnc-account-get-full-name acct))
                (accounts-total (cdr splits) (+ total (* (xaccSplitGetValue (car splits)) -1)) acct) 
                (accounts-total (cdr splits) total acct)) 
              (accounts-total (cdr splits) total acct))))
      
        (define (accounts-total-last-year splits total acct)
          (if (null? splits)
            total
            ;; is this number really a year? It seems to be longer 2020
            (if (and (> (xaccTransGetDate (xaccSplitGetParent (car splits))) (- start-date 31556952)) (< (xaccTransGetDate (xaccSplitGetParent (car splits))) (- end-date 31556952)))
               (if (string-contains (gnc-account-get-full-name (xaccSplitGetAccount (car splits))) (gnc-account-get-full-name acct))
                (accounts-total-last-year (cdr splits) (+ total (* (xaccSplitGetValue (car splits)) -1)) acct) 
                (accounts-total-last-year (cdr splits) total acct)) 
              (accounts-total-last-year (cdr splits) total acct))))

        ;; Here's where we fill the report document with content.  We
      ;; do this by adding 'html objects' such as text, tables, and
      ;; graphs to the html document we already created.
      
      ;; the report's style sheet (an "invisible" option that every
      ;; report has) will usually set the default background color,
      ;; but we can override that here.  You set background color in
      ;; HTML by specifying the "bgcolor" attribute for the <body>
      ;; tag.

      ;; every HTML object has "styles" for markup and data.  the
      ;; style for an HTML tag such as "body" tells the HTML
      ;; document how to render the markup and content for tagged
      ;; elements.  For each tag, you can specify a font-face,
      ;; font-color, and font-size to render the contents of the
      ;; element, and any number of attributes to put in the
      ;; start-tag.  You can pass 'inheritable? #f if you wish the
      ;; style to apply only to markup in the object itself and not
      ;; to its components.  You can also override the tag itself if
      ;; you want to create your own custom markup (see
      ;; documentation).
      
      ;; in this case, we are saying "every time you see <body>
      ;; markup anywhere in 'document' or its components, add the
      ;; attribute "bgcolor=0xXXXXXX" in the start tag, and enclose
      ;; the content in a <font> block to set the font color".
      ;; Altogether, we get
      ;;
      ;; <body bgcolor=0xXXXXXXX>
      ;; <font color="0xXXXXXX"> (body) </font>
      ;; </body>

      ;; of course if a component object explicitly selects a 
      ;; different font that will override the body font.
      
      (gnc:html-document-set-style!
       document "body" 
       'attribute (list "bgcolor" (gnc:color-option->html bg-color-op)))
      
      ;; the title of the report will be rendered by the 
      ;; selected style sheet.  All we have to do is set it in the
      ;; HTML document.
      
      ;; Note we invoke the _ function upon this string.
      ;; The _ function works the same way as in C -- if a
      ;; translation of the given string is available for the
      ;; current locale, then the translation is returned,
      ;; otherwise the original string is returned.
      (gnc:html-document-set-title! document (G_ "Church Accounts"))

;; TODO - group subaccounts correctly so parents are above child accounts and child accounts are indented
;; split out pastoral fund support more
;;balaz and detti internal transfer - split out what is from pastoral fund and what from general
;;break down detail for specific and sundry
;;show a bit more detail on weekend away
;; present this landscape as well? One page per account

  (map 
    (lambda(acct)
      (gnc:html-document-add-object!
        document 
        (gnc:make-html-text
          (gnc:html-markup-h1
            (xaccAccountGetName acct))
          (gnc:html-markup-p 
            (gnc:html-markup/format 
              (G_ "Balance brought forward: ~a")
              (xaccPrintAmount 
                (balanceincdesc acct start-date) 
                (gnc-default-print-info #f))))))
      (let 
        ((table (gnc:make-html-table)))
 (gnc:html-table-append-column! 
          table
          (append 
            (list 
              (gnc:make-html-table-cell 
                (gnc:make-html-text 
                  (gnc:html-markup-h2 (G_ "Income")))))
            (map 
              (lambda(item) 
                (gnc:make-html-table-cell
                  (xaccAccountGetName item)
                    (gnc:make-html-text
                    (gnc:html-markup-br)))) 
               (append (filter (lambda(item) (string-contains (gnc-account-get-full-name item) "Income")) (gnc-account-get-children acct)) (filtered-accounts (gnc-account-get-children acct) '() "Income")))))

        (gnc:html-table-append-column! 
          table
          (append
            (list 
              (gnc:make-html-table-cell 
                (gnc:make-html-text 
                  (gnc:html-markup-h2 (format #f (G_ "~a") (gnc-print-time64 start-date "%Y"))))))
            (map
              (lambda(item)
                (gnc:make-html-table-cell
                  (xaccPrintAmount
                    (apply + 
                      (map 
                        (lambda (sum-account)
                          (accounts-total (xaccAccountGetSplitList sum-account) 0 item))
                        (append (gnc-account-get-descendants item) (list item)))) ;;include the current account as well as descendants
                  (gnc-default-print-info #f))))
              (append (filter (lambda(item) (string-contains (gnc-account-get-full-name item) "Income")) (gnc-account-get-children acct)) (filtered-accounts (gnc-account-get-children acct) '() "Income")))))
;;make a table each time with an income column and expenditure column
(gnc:html-table-append-column! 
          table
          (append
            (list 
              (gnc:make-html-table-cell 
                (gnc:make-html-text 
                  (gnc:html-markup-h2 (format #f (G_ "~a") (gnc-print-time64 (- start-date 31470552) "%Y")))))) ;;the start date less one day short of a year
            (map
              (lambda(item)
                (gnc:make-html-table-cell
                  (xaccPrintAmount
                    (apply + 
                      (map 
                        (lambda (sum-account)
                          (accounts-total-last-year (xaccAccountGetSplitList sum-account) 0 item))
                        (append (gnc-account-get-descendants item) (list item)))) ;;include the current account as well as descendants
                  (gnc-default-print-info #f))))
             (append (filter (lambda(item) (string-contains (gnc-account-get-full-name item) "Income")) (gnc-account-get-children acct)) (filtered-accounts (gnc-account-get-children acct) '() "Income")))))

        (gnc:html-table-append-column! 
          table
          (append
            (list 
              (gnc:make-html-table-cell 
                (gnc:make-html-text 
                  (gnc:html-markup-h2 (G_ "Internal Transfer")))))
            (map 
              (lambda(item) 
                (gnc:make-html-table-cell
                (map 
                   (lambda (account-match)
                       
                     account-match
                   ;; (gnc:make-html-text (gnc:html-markup-br))
                       )
                   (remove-duplicates (map (lambda (split) (xaccSplitGetCorrAccountFullName split)) (xaccAccountGetSplitList item)) '()))
                  
                (xaccAccountGetName item)
                  (gnc:make-html-text
                    (gnc:html-markup-br)))) 
              (append (filter (lambda(item) (string-contains (gnc-account-get-full-name item) "Internal Transfer")) (gnc-account-get-children acct)) (filtered-accounts (gnc-account-get-children acct) '() "Internal Transfer")))
            (gnc:html-make-empty-cells 20))) ;;we make these empty cells so the income cells show correctly - don't know why this is necessary
(gnc:html-table-append-column! 
          table
          (append
            (list 
              (gnc:make-html-table-cell 
                (gnc:make-html-text 
                  (gnc:html-markup-h2 (format #f (G_ "~a") (gnc-print-time64 start-date "%Y"))))))
            (map
              (lambda(item)
                ;; if the general account then list amounts sent to each account separately
                ;; no descendants needed because internal transfers doesn't have them
                (gnc:make-html-table-cell
                (map 
                   (lambda 
                     (account-match)
                     (xaccPrintAmount
                         (accounts-total 
                           (filter 
                             (lambda(filt) 
                               (string-contains account-match (xaccSplitGetCorrAccountFullName filt))) (xaccAccountGetSplitList item)) 0 item)
                         (gnc-default-print-info #f))
                   ;; (gnc:make-html-text
                   ;; (gnc:html-markup-br))
                       )
                   (remove-duplicates (map (lambda (split) (xaccSplitGetCorrAccountFullName split)) (xaccAccountGetSplitList item)) '()))
                ;; don't show this rolled up - show it per other account, per account sent to or received from
                  ;;create a list of all corresponding accounts from splits, remove duplicates then map to it
                 

                  (xaccPrintAmount
                    (apply + 
                      (map 
                        (lambda (sum-account)
                          (accounts-total (xaccAccountGetSplitList sum-account) 0 item))
                        (append (gnc-account-get-descendants item) (list item)))) ;;include the current account as well as descendants
                  (gnc-default-print-info #f))))
              (append (filter (lambda(item) (string-contains (gnc-account-get-full-name item) "Internal Transfer")) (gnc-account-get-children acct)) (filtered-accounts (gnc-account-get-children acct) '() "Internal Transfer")))
            (gnc:html-make-empty-cells 20)))
       (gnc:html-table-append-column! 
          table
          (append
            (list 
              (gnc:make-html-table-cell 
                (gnc:make-html-text 
                  (gnc:html-markup-h2 (format #f (G_ "~a") (gnc-print-time64 (- start-date 31516952) "%Y"))))))
            (map
              (lambda(item)
                (gnc:make-html-table-cell
                 (map 
                   (lambda 
                     (account-match)
                     (xaccPrintAmount
                         (accounts-total-last-year
                           (filter 
                             (lambda(filt) 
                               (string-contains account-match (xaccSplitGetCorrAccountFullName filt))) (xaccAccountGetSplitList item)) 0 item)
                         (gnc-default-print-info #f))
                   ;; (gnc:make-html-text
                   ;; (gnc:html-markup-br))
                       )
                   (remove-duplicates (map (lambda (split) (xaccSplitGetCorrAccountFullName split)) (xaccAccountGetSplitList item)) '()))

                  (xaccPrintAmount
                    (apply + 
                      (map 
                        (lambda (sum-account)
                          ;;if I get a list of all the accounts amounts are being transferred to here 
                          ;;i can show different rows
                          (accounts-total-last-year (xaccAccountGetSplitList sum-account) 0 item))
                        (append (gnc-account-get-descendants item) (list item)))) ;;include the current account as well as descendants
                  (gnc-default-print-info #f))))
              (append (filter (lambda(item) (string-contains (gnc-account-get-full-name item) "Internal Transfer")) (gnc-account-get-children acct)) (filtered-accounts (gnc-account-get-children acct) '() "Internal Transfer")))(gnc:html-make-empty-cells 20)))



(gnc:html-table-append-column! 
          table
          (append 
            (list 
              (gnc:make-html-table-cell 
                (gnc:make-html-text 
                  (gnc:html-markup-h2 (G_ "Expenditure")))))
            (map 
              (lambda(item)
                ;;want an if statement here for general account
                ;;if acct is general account, and item is Employment we want to look through and add up all items with neil, ed, matt in.
                ;;we also want to split out all the donations to other ministries

                ;;if statement for missions account, want all expenditure grouped by missionary
                (gnc:make-html-table-cell
                  (if (> (gnc-account-get-current-depth item) 1) (gnc:make-html-text (G_ "-")) #f)
                  (if (> (gnc-account-get-current-depth item) 2) (gnc:make-html-text (G_ "-")) #f)
                  (if (> (gnc-account-get-current-depth item) 3) (gnc:make-html-text (G_ "-")) #f)
                  (if (> (gnc-account-get-current-depth item) 4) (gnc:make-html-text (G_ "-")) #f)
                  (xaccAccountGetName item)
                    (gnc:make-html-text
                    (gnc:html-markup-br)))) 
              (append (filter (lambda(item) (string-contains (gnc-account-get-full-name item) "Expenditure")) (gnc-account-get-children acct)) (filtered-accounts (gnc-account-get-children acct) '() "Expenditure")))))

        (gnc:html-table-append-column! 
          table
          (append
            (list 
              (gnc:make-html-table-cell 
                (gnc:make-html-text 
                  (gnc:html-markup-h2 (format #f (G_ "~a") (gnc-print-time64 start-date "%Y"))))))
            (map
              (lambda(item)
                (gnc:make-html-table-cell
                  (xaccPrintAmount
                    (apply + 
                      (map 
                        (lambda (sum-account)
                          (accounts-total (xaccAccountGetSplitList sum-account) 0 item))
                        (append (gnc-account-get-descendants item) (list item)))) ;;include the current account as well as descendants
                  (gnc-default-print-info #f))))
              (append (filter (lambda(item) (string-contains (gnc-account-get-full-name item) "Expenditure")) (gnc-account-get-children acct)) (filtered-accounts (gnc-account-get-children acct) '() "Expenditure")))))
;;make a table each time with an income column and expenditure column
(gnc:html-table-append-column! 
          table
          (append
            (list 
              (gnc:make-html-table-cell 
                (gnc:make-html-text 
                  (gnc:html-markup-h2 (format #f (G_ "~a") (gnc-print-time64 (- start-date 31516952) "%Y"))))))
            (map
              (lambda(item)
                (gnc:make-html-table-cell
                  (xaccPrintAmount
                    (apply + 
                      (map 
                        (lambda (sum-account)
                          (accounts-total-last-year (xaccAccountGetSplitList sum-account) 0 item))
                        (append (gnc-account-get-descendants item) (list item)))) ;;include the current account as well as descendants
                  (gnc-default-print-info #f))))
              (append (filter (lambda(item) (string-contains (gnc-account-get-full-name item) "Expenditure")) (gnc-account-get-children acct)) (filtered-accounts (gnc-account-get-children acct) '() "Expenditure")))))

          (gnc:html-table-set-style! table "table"
                                     'attribute (list "style" "width:120em"))
          (gnc:html-document-add-object! document table))
          (gnc:html-document-add-object! document
            (gnc:make-html-text
            (gnc:html-markup-p 
            (gnc:html-markup/format 
              (G_ "Closing Balance at ~a:   ")
              end-date-string)
              (xaccPrintAmount 
                (balanceincdesc acct end-date) 
                (gnc-default-print-info #f))))))
    accounts)
      
        
      ;; here's a bullet list of accounts.  We can mark up the
      ;; account name with an <a></a> anchor with a special HREF to
      ;; open a Gnucash register when the link is clicked.  What you
      ;; need to do is pass the HREF "gnc-register:account=My
      ;; Account Name" to html-markup-anchor.  The account name
      ;; passed must be the "full" account name that you get from
      ;; gnc-account-get-full-name.  You should build this url using
      ;; (gnc-build-url ...)
      ;;
      ;; html-markup-anchor takes the link to jump to as its first
      ;; arg and then puts the remaining args in the body of the
      ;; link).
      ;;
      ;; html-markup-ul makes a "<ul>" unnumbered list, and takes as
      ;; its one argument a list of items to put in <li> blocks.
            
      document)))

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 
 ;; The version of this report.
 'version 1
 
 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name (N_ "Account Extended")

 ;; The GUID for this report. This string should be unique, set once
 ;; and left alone forever after that. In theory, you could use any
 ;; unique string, even a meaningful one (!) but its probably best to
 ;; use a true uuid. Get them from `uuidgen | sed -e s/-//g` and paste
 ;; the results in here. You must make a new guid for each report!
 'report-guid "5786de5a6e5344f186b4c9ee602e0a35"

 ;; The name in the menu
 ;; (only necessary if it differs from the name)
 'menu-name (N_ "Sample Report with Examples")

 ;; A tip that is used to provide additional information about the
 ;; report to the user.
 'menu-tip (N_ "A sample report with examples.")

 ;; A path describing where to put the report in the menu system.
 ;; In this case, it's going under the utility menu.
 'menu-path (list gnc:menuname-example)

 ;; The options generator function defined above.
 'options-generator options-generator
 
 ;; The rendering function defined above.
 'renderer account-extended-renderer)
