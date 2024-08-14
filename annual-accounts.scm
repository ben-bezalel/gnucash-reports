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

;;TODO
;;refactoring each table to use less copy paste, more general functions - make a list
;;right aligning all cost information in table
;;putting pound signs (or whatever currency sign is selected) in. 
;;possibly removing totals from internal transfer and possibly shortening so both sides of a transaction (the to and from account) are on the same line 
;;write brief guide about how to structure accounts to make this report work, and how to select accounts to include in it.
;;consistency on whether zeros are shown in the totals rows and columns

(define-module (gnucash reports example annual-accounts))

(use-modules (gnucash engine))
(use-modules (gnucash utilities)) 
(use-modules (gnucash core-utils))
(use-modules (gnucash app-utils))
(use-modules (gnucash report))
(use-modules (srfi srfi-1))
(debug-enable 'backtrace)

(define (list-contains item lst)
  (if (pair? lst)
    (if (pair? (car lst))
      (if (equals? item (car lst))
        #t
        (list-contains item (cdr lst)))
      #f)
    #f))

;; this recursive function sums the balances of sub-accounts into a parent acoount.
;; it inverts the sign of the result. A single number is returned. 
(define (balanceincdesc acct date)
  (+ 
    (apply + 
      (map
	(lambda(acctdescendants)
	  (* (xaccAccountGetBalanceAsOfDate acctdescendants date) -1))
	    (gnc-account-get-descendants acct)))
    (* (xaccAccountGetBalanceAsOfDate acct date) -1)))

;;this recursive function returns true if the account or any of its parents
;;contain the word Income in the account name
(define (parent-check acct searchstr)	    
  (if (string-contains (gnc-account-get-full-name acct) searchstr)
    #t
    (if (not (gnc-account-is-root acct))
      (parent-check (gnc-account-get-parent acct) searchstr)
      #f)))

(define (level-one-print acct)	    
  (if (> (gnc-account-get-current-depth acct) 1)
    (level-one-print (gnc-account-get-parent acct))
    acct))

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
              (lambda(x) (not (equal? (xaccAccountGetName(car lst)) x))) 
              (map 
                (lambda(acct) (xaccAccountGetName acct)) 
                (cdr lst)))) 
          (length 
            (map 
              (lambda(acct) (xaccAccountGetName acct)) 
              (cdr lst))))    
        (remove-duplicates (cdr lst) (cons (xaccAccountGetName (car lst)) removed)) 
        (remove-duplicates (cdr lst) removed))))

;; This function will generate a set of options that GnuCash
;; will use to display a dialog where the user can select
;; values for your report's parameters.
(define (options-generator)    
  (let* ((options (gnc:new-options)) 
         ;; This is a helper function for making options.
         ;; See libgnucash/app-utils/options.scm for details.
         (add-option 
          (lambda (new-option)
            (gnc:register-option options new-option))))
    
    (add-option
     (gnc:make-date-option
      (N_ "General") (N_ "Start Date")
      "a" (N_ "This is a date option.")
      (lambda () (cons 'absolute (current-time)))
      #f 'absolute #f ))
    ;; the make-date-option 
    ;; first parameter - the tab this option is displayed in
    ;; second parameter - the name of the option on the tab
    ;; third parameter - sorting key - the order it comes ... 
    ;; fourth parameter - help/tooltip text
    ;; fifth parameter - the default value- today's date
    ;; sixth parameter - if true time is shown, if false time isn't shown
    ;; seventh parameter - absolute time, or relative time from a number of options

    (add-option
     (gnc:make-date-option
      (N_ "General") (N_ "End Date")
      "b" (N_ "This is a date option.")
      (lambda () (cons 'absolute (current-time)))
      #f 'absolute #f ))
    
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
      (lambda () '())
      #f #t))

    (gnc:options-set-default-section options "General")      
    options))

;; This is the rendering function. It accepts a database of options
;; and generates an object of type <html-document>.  See the file
;; report-html.txt for documentation; the file report-html.scm
;; includes all the relevant Scheme code. The option database passed
;; to the function is one created by the options-generator function
;; defined above.
(define (annual-accounts-renderer report-obj)
  ;; These are some helper functions for looking up option values.
  (define (get-op section name)
    (gnc:lookup-option (gnc:report-options report-obj) section name))
  
  (define (op-value section name)
    (gnc:option-value (get-op section name)))

  ;; The first thing we do is make local variables for all the specific
  ;; options in the set of options given to the function. This set will
  ;; be generated by the options generator above.
  (let ((start-date     (gnc:date-option-absolute-time
                       (op-value "General" "Start Date")))
        (end-date    (gnc:date-option-absolute-time
                       (op-value "General" "End Date")))
        (accounts     (op-value "Accounts" "An account list option"))
        
	(table (gnc:make-html-table))
	(incometable (gnc:make-html-table))
        (expendituretable (gnc:make-html-table))
        (internaltransfertable (gnc:make-html-table))
        (document (gnc:make-html-document)))

    ;; these are samples of different date options. for a simple
    ;; date with day, month, and year but no time you should use
    ;; qof-print-date
    (let ((start-date-str (gnc-print-time64 start-date "%Y"))
          (end-date-str (gnc-print-time64 end-date "%x %X"))
          (income-accounts 
            (remove-duplicates 
              (filter 
                (lambda(item) 
                  (and 
                    (<= (gnc-account-get-current-depth item) 3) 
                      (or 
                        (null? 
                          (gnc-account-get-descendants item)) 
                        (= 
                          (gnc-account-get-current-depth item) 3)))) 
                (filtered-accounts accounts '() "Income"))'()))
          (internal-transfer-accounts 
            (remove-duplicates 
              (map 
                (lambda(item) 
                  (level-one-print item))
                   (filter 
                     (lambda(item) 
                       (and 
                         (<= (gnc-account-get-current-depth item) 3) 
                         (or 
                           (null? 
                             (gnc-account-get-descendants item)) 
                           (= 
                             (gnc-account-get-current-depth item) 3)))) 
                     (filtered-accounts accounts '() "Internal Transfer")))'())))

      ;; Here's where we fill the report document with content.  We
      ;; do this by adding 'html objects' such as text, tables, and
      ;; graphs to the html document we already created.
     
      (define (accounts-total splits total acct)
       (if (null? splits)
          total
           (if (and (> (xaccTransGetDate (xaccSplitGetParent (car splits))) start-date) (< (xaccTransGetDate (xaccSplitGetParent (car splits))) end-date))
           (if (string-contains (gnc-account-get-full-name (xaccSplitGetAccount (car splits))) (gnc-account-get-full-name acct))
                  (accounts-total (cdr splits) (+ total (* (xaccSplitGetValue (car splits)) -1)) acct) 
                  (accounts-total (cdr splits) total acct)) 
              (accounts-total (cdr splits) total acct)
           )
        )
      )

(define (internal-transfer-accounts-total splits total acct other)
  (if (null? splits)
    total
    (if (and (> (xaccTransGetDate (xaccSplitGetParent (car splits))) start-date) (< (xaccTransGetDate (xaccSplitGetParent (car splits))) end-date))
      (if (string-contains (gnc-account-get-full-name (xaccSplitGetAccount (car splits))) (gnc-account-get-full-name acct))
        (if (string-contains (xaccSplitGetCorrAccountFullName (car splits)) other)
          (internal-transfer-accounts-total (cdr splits) (+ total (* (xaccSplitGetValue (car splits)) -1)) acct other) 
          (internal-transfer-accounts-total (cdr splits) total acct other))
        (internal-transfer-accounts-total (cdr splits) total acct other)) 
      (internal-transfer-accounts-total (cdr splits) total acct other))))

      (gnc:html-document-set-title! document (format #f (G_ "Annual Accounts for ~a") (gnc-print-time64 start-date "%Y")))

      ;; somewhere I need to tell the html table it has two columns
      ;; first column of descriptions
      ;; last column of totals
           
        ;; looks like it is easy to export to csv using trep-engine - probably useful for sankey diagram
       
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
      
      ;;what if some but not all accounts in a tree are selected?
      ;;probably need to build up own data structure of accounts to create figures for

      ;;report-utilities.scm has quite a few useful functions. getting balances for accounts over intervals - this would mean i don't need to look at every transaction

      ;;what does gnc:accounts-and-all-descendents return? account-get-balance-interval get-assoc-account-balances-total

      ;;I think my report will be select the accounts you want, all children are included by default
      ;;so for each account, get all the children accounts, get all the transactions in the time period, split them down by month, sum them, add to parent account
     
      ;;maybe the best way to do this in the future is to just have income accounts and expense accounts, and rely on reporting to make +/- consistent 

      (gnc:html-document-add-object!
        document
       (gnc:make-html-text 
        (gnc:html-markup-h1
          (G_ "Income"))))
      
      (gnc:html-table-append-column! ;;this is the first column containing row descriptions
        incometable    
        (append 
          (list 
            (gnc:make-html-table-cell (G_ " "))) 
          (map 
            (lambda(acct)
              (gnc:make-html-table-cell (G_ acct)))
            (remove-duplicates
              (filter 
                (lambda(item) 
                  (and 
                    (<= (gnc-account-get-current-depth item) 3) 
                    (or 
                      (null? (gnc-account-get-descendants item)) 
                      (= 
                        (gnc-account-get-current-depth item) 
                        3)))) 
                (filtered-accounts accounts '() "Income"))
              '()))
            (list 
              (gnc:make-html-table-cell (G_ "Totals"))))) 
              
      ;;should I alphabetise the sorted list?        
      ;;this is building the income table contents. The output is a column for each account, added via mutation to the html of the document
      (map ;;these are the values in the table
        (lambda(income-column)
          (gnc:html-table-append-column! 
            incometable
            (append ;;this append allows us to add a column title to the top cell 
              (list (gnc:make-html-table-cell (gnc-account-get-full-name income-column))) 
              (map ;;this map runs through third level accounts with "Income" in the title of the account, or a parent account. Duplicate accounts with the same name are removed.  
                (lambda(income-row)
                  (gnc:make-html-table-cell
                    (if (= ;;if the value isn't zero then print it
                          (apply + 
                            (map ;;this finds the single account that needs mapped
                              (lambda(accounts-to-sum)
                                (apply + 
                                  (map 
                                    (lambda (sum-account)
                                      (accounts-total (xaccAccountGetSplitList sum-account) 0 accounts-to-sum))
                                    (gnc-account-get-descendants income-column))))  
                              (filter
                                (lambda(accounts-to-sum)
                                  (and (string-contains income-row (xaccAccountGetName accounts-to-sum)) (string-contains (gnc-account-get-full-name accounts-to-sum) (gnc-account-get-full-name income-column))))
                                (filter 
                                  (lambda(item) 
                                    (and (<= (gnc-account-get-current-depth item) 3) (or (null? (gnc-account-get-descendants item)) (= (gnc-account-get-current-depth item) 3)))) 
                                  (filtered-accounts accounts '() "Income"))))) 0)
                      (G_ " ")
                      (xaccPrintAmount
                        (apply + 
                          (map ;;this finds the single account that needs mapped
                            (lambda(accounts-to-sum)
                            (apply + 
                              (map 
                                (lambda (sum-account)
                                  (accounts-total (xaccAccountGetSplitList sum-account) 0 accounts-to-sum))
                                (gnc-account-get-descendants income-column))))  
                           (filter
                             (lambda(accounts-to-sum)
                               (and (string-contains income-row (xaccAccountGetName accounts-to-sum)) (string-contains (gnc-account-get-full-name accounts-to-sum) (gnc-account-get-full-name income-column))))
                             (filter 
                               (lambda(item) 
                                 (and (<= (gnc-account-get-current-depth item) 3) 
                                   (or (null? (gnc-account-get-descendants item)) (= (gnc-account-get-current-depth item) 3)))) 
                               (filtered-accounts accounts '() "Income")))))
                        (gnc-default-print-info #f)))))
                income-accounts)        
                (list (gnc:make-html-table-cell ;;column totals
                  (if (= ;;if the value isn't zero then print it
                        (apply + 
                          (map ;;this finds the single account that needs mapped
                            (lambda(accounts-to-sum)
                              (apply + 
                                (map 
                                  (lambda (sum-account)
                                    (accounts-total (xaccAccountGetSplitList sum-account) 0 accounts-to-sum))
                                  (gnc-account-get-descendants income-column))))  
                            (filter
                              (lambda(accounts-to-sum)
                                 (string-contains (gnc-account-get-full-name accounts-to-sum) (gnc-account-get-full-name income-column)))
                              (filter 
                                (lambda(item) 
                                  (and (<= (gnc-account-get-current-depth item) 3) (or (null? (gnc-account-get-descendants item)) (= (gnc-account-get-current-depth item) 3)))) 
                                (filtered-accounts accounts '() "Income"))))) 0)
                    (G_ " ")
                    (xaccPrintAmount
                      (apply + 
                        (map ;;this finds the single account that needs mapped
                          (lambda(accounts-to-sum)
                          (apply + 
                            (map 
                              (lambda (sum-account)
                                (accounts-total (xaccAccountGetSplitList sum-account) 0 accounts-to-sum))
                              (gnc-account-get-descendants income-column))))  
                         (filter
                           (lambda(accounts-to-sum)
                             (string-contains (gnc-account-get-full-name accounts-to-sum) (gnc-account-get-full-name income-column)))
                           (filter 
                             (lambda(item) 
                               (and (<= (gnc-account-get-current-depth item) 3) 
                                 (or (null? (gnc-account-get-descendants item)) (= (gnc-account-get-current-depth item) 3)))) 
                             (filtered-accounts accounts '() "Income")))))
                      (gnc-default-print-info #f))))))))
        accounts)

      (gnc:html-table-append-column! ;;this final column shows the row totals and final total at the bottom
        incometable    
        (append 
          (list 
            (gnc:make-html-table-cell (G_ "Totals"))) 
          (map ;;this map runs through third level accounts with "Income" in the title of the account, or a parent account. Duplicate accounts with the same name are removed.  
            (lambda(income-row)
              (gnc:make-html-table-cell
                (if (= ;;if the value isn't zero then print it
                  (apply + 
                    (map ;;this finds the single account that needs mapped
                      (lambda(accounts-to-sum)
                        (apply +
                          (map
                            (lambda (income-column)
                              (apply + 
                                (map 
                                  (lambda (sum-account)
                                    (accounts-total (xaccAccountGetSplitList sum-account) 0 accounts-to-sum))
                                (gnc-account-get-descendants income-column))))
                            accounts)))
                      (filter
                        (lambda(accounts-to-sum)
                          (string-contains income-row (xaccAccountGetName accounts-to-sum)) )
                            (filter 
                              (lambda(item) 
                                (and (<= (gnc-account-get-current-depth item) 3) (or (null? (gnc-account-get-descendants item)) (= (gnc-account-get-current-depth item) 3)))) 
                                  (filtered-accounts accounts '() "Income"))))) 0)
                  (G_ " ")
                  (xaccPrintAmount
                    (apply + 
                      (map ;;this finds the single account that needs mapped
                        (lambda(accounts-to-sum)
                          (apply +
                            (map
                              (lambda (income-column)
                                (apply + 
                                  (map 
                                    (lambda (sum-account)
                                      (accounts-total (xaccAccountGetSplitList sum-account) 0 accounts-to-sum))
                                    (gnc-account-get-descendants income-column))))
                              accounts)))
                      (filter
                        (lambda(accounts-to-sum)
                          (string-contains income-row (xaccAccountGetName accounts-to-sum)) )
                            (filter 
                              (lambda(item) 
                                (and (<= (gnc-account-get-current-depth item) 3) 
                                  (or (null? (gnc-account-get-descendants item)) (= (gnc-account-get-current-depth item) 3)))) 
                                   (filtered-accounts accounts '() "Income")))))
                    (gnc-default-print-info #f)))))
                income-accounts)
              (list (gnc:make-html-table-cell
                  (xaccPrintAmount
                    (apply + 
                      (map ;;this finds the single account that needs mapped
                        (lambda(accounts-to-sum)
                          (apply +
                            (map
                              (lambda (income-column)
                                (apply + 
                                  (map 
                                    (lambda (sum-account)
                                      (accounts-total (xaccAccountGetSplitList sum-account) 0 accounts-to-sum))
                                    (gnc-account-get-descendants income-column))))
                              accounts)))
                            (filter 
                              (lambda(item) 
                                (and (<= (gnc-account-get-current-depth item) 3) 
                                  (or (null? (gnc-account-get-descendants item)) (= (gnc-account-get-current-depth item) 3)))) 
                                   (filtered-accounts accounts '() "Income"))))
                    (gnc-default-print-info #f))))))

          (gnc:html-table-set-style! incometable "table"
                                     'attribute (list "style" "width:100%; max-width:120em")
                                     'attribute (list "cellpadding" "0"))
          (gnc:html-document-add-object!
            document incometable)

          (gnc:html-document-add-object!
            document
            (gnc:make-html-text 
              (gnc:html-markup-h1
                (G_ "Internal Transfers"))))
          (gnc:html-table-append-column!
            internaltransfertable    
            (append 
              (list (gnc:make-html-table-cell (G_ " "))) 
              (map 
                (lambda(acct)
                  (gnc:make-html-table-cell (G_ acct)))
                internal-transfer-accounts)
              (list (gnc:make-html-table-cell (G_ "Totals")))))
          (map
            (lambda(acct)
              (gnc:html-table-append-column! 
                internaltransfertable
                (append 
                  (list(gnc:make-html-table-cell (gnc-account-get-full-name acct))) 
                  (map 
                    (lambda(internal-transfer-row)
                      (gnc:make-html-table-cell
                        (xaccPrintAmount
                         (if (=  (apply +
                            (map
                              (lambda(subacct)
                                (internal-transfer-accounts-total (xaccAccountGetSplitList subacct) 0 acct internal-transfer-row))
                              (filtered-accounts accounts '() "Internal Transfer"))) 0)
                             (G_ " ")
                          (apply +
                            (map
                              (lambda(subacct)
                                (internal-transfer-accounts-total (xaccAccountGetSplitList subacct) 0 acct internal-transfer-row))
                              (filtered-accounts accounts '() "Internal Transfer"))))
                          (gnc-default-print-info #f))))
                    internal-transfer-accounts)
                  (list
                    (gnc:make-html-table-cell
                        (xaccPrintAmount
                          (apply + 
                            (map (lambda(internal-transfer-row) 
                          (apply +
                            (map
                              (lambda(subacct)
                                (internal-transfer-accounts-total (xaccAccountGetSplitList subacct) 0 acct internal-transfer-row))
                              (filtered-accounts accounts '() "Internal Transfer"))))
                                 internal-transfer-accounts))
                          (gnc-default-print-info #f))))))) ;;do internal transfer accounts that takes duplicates out? cycle through actual accounts and the names
            accounts)
              
          (gnc:html-table-append-column!
            internaltransfertable
            (append 
              (list(gnc:make-html-table-cell (G_ "Totals"))) 
              (map 
                (lambda(internal-transfer-row)
                  (gnc:make-html-table-cell
                    (xaccPrintAmount
                      (apply + 
                        (map 
                          (lambda(acct)          
                            (apply +
                              (map
                                (lambda(subacct)
                                  (internal-transfer-accounts-total (xaccAccountGetSplitList subacct) 0 acct internal-transfer-row))
                                  (filtered-accounts accounts '() "Internal Transfer")))) 
                          accounts))
                      (gnc-default-print-info #f))))
                internal-transfer-accounts)
              (list  
                (gnc:make-html-table-cell
                  (xaccPrintAmount
                    (apply + 
                      (map 
                        (lambda(internal-transfer-row)
                          (apply + 
                            (map 
                              (lambda(acct)
                                (apply +
                                  (map
                                    (lambda(subacct)
                                      (internal-transfer-accounts-total (xaccAccountGetSplitList subacct) 0 acct internal-transfer-row))
                                  (filtered-accounts accounts '() "Internal Transfer")))) 
                              accounts)))
                        internal-transfer-accounts))
                    (gnc-default-print-info #f)))))) ;;do internal transfer accounts that takes duplicates out? cycle through actual accounts and the names
                
          (gnc:html-table-set-style! internaltransfertable "table"
                                     'attribute (list "style" "width:100%; max-width:120em")
                                     'attribute (list "cellpadding" "0"))
          (gnc:html-document-add-object!
            document internaltransfertable)

          (gnc:html-document-add-object!
            document
            (gnc:make-html-text 
              (gnc:html-markup-h1
                (G_ "Expenditure"))))

          (gnc:html-table-append-column!
            expendituretable    
            (append 
              (list (gnc:make-html-table-cell (G_ " "))) 
              (map 
                (lambda(acct)
                  (gnc:make-html-table-cell
                    (if (equal? (xaccAccountGetName acct) "Expenditure")
                        (xaccAccountGetName (level-one-print acct))
                        (xaccAccountGetName acct))))
                (filter 
                  (lambda(item) 
                    (and 
                      (<= (gnc-account-get-current-depth item) 3) 
                      (or 
                        (null? (gnc-account-get-descendants item)) 
                        (= (gnc-account-get-current-depth item) 3)))) 
                  (filtered-accounts accounts '() "Expenditure"))) 
              (list (gnc:make-html-table-cell (G_ "Totals")))))
          ;; it would probably be better to re-implement the filtered-accounts 
          ;;function using filter and predicate with string-contains

          (map
            (lambda(acct)
              (gnc:html-table-append-column! 
                expendituretable
                (append 
                  (list(gnc:make-html-table-cell (gnc-account-get-full-name acct))) 
                  (map 
                    (lambda(subacct)
                      (gnc:make-html-table-cell
                        (if (not (= (apply + (map (lambda (subsubacct) (accounts-total (xaccAccountGetSplitList subsubacct) 0 subacct)) (gnc-account-get-descendants acct))) 0)) 
                            (apply + 
                              (map 
                                (lambda (subsubacct)
                                  (accounts-total (xaccAccountGetSplitList subsubacct) 0 subacct))
                                (gnc-account-get-descendants acct)))
                            (G_ " "))))
                    (filter 
                      (lambda(item) 
                        (and 
                          (<= (gnc-account-get-current-depth item) 3) 
                          (or (null? (gnc-account-get-descendants item)) (= (gnc-account-get-current-depth item) 3)))) 
                      (filtered-accounts accounts '() "Expenditure")))    
                  (list    
                    (gnc:make-html-table-cell
                      (apply + 
                        (map 
                          (lambda(subacct)
                            (apply + 
                              (map 
                                (lambda (subsubacct)
                                  (accounts-total (xaccAccountGetSplitList subsubacct) 0 subacct))
                                (gnc-account-get-descendants acct))))
                          (filter 
                            (lambda(item) 
                              (and (<= (gnc-account-get-current-depth item) 3) 
                              (or (null? (gnc-account-get-descendants item)) (= (gnc-account-get-current-depth item) 3)))) 
                            (filtered-accounts accounts '() "Expenditure")))))))))
              accounts)

            (gnc:html-table-append-column!
              expendituretable    
              (append 
                (list (gnc:make-html-table-cell (G_ "Totals")))
                (map 
                  (lambda(subacct)
                    (gnc:make-html-table-cell
                      (apply + 
                        (map 
                          (lambda(acct)
                            (if (not (= (apply + (map (lambda (subsubacct) (accounts-total (xaccAccountGetSplitList subsubacct) 0 subacct)) (gnc-account-get-descendants acct))) 0)) 
                              (apply + 
                                (map 
                                  (lambda (subsubacct)
                                    (accounts-total (xaccAccountGetSplitList subsubacct) 0 subacct))
                                  (gnc-account-get-descendants acct)))
                              0)) 
                          accounts))))
                  (filter 
                    (lambda(item) 
                      (and 
                        (<= (gnc-account-get-current-depth item) 3) 
                        (or 
                          (null? (gnc-account-get-descendants item)) 
                          (= (gnc-account-get-current-depth item) 3)))) 
                    (filtered-accounts accounts '() "Expenditure")))
                (list 
                  (gnc:make-html-table-cell
                    (apply + 
                      (map 
                        (lambda(subacct)
                          (apply + 
                            (map 
                              (lambda(acct)
                                (if (not (= (apply + (map (lambda (subsubacct) (accounts-total (xaccAccountGetSplitList subsubacct) 0 subacct)) (gnc-account-get-descendants acct))) 0)) 
                                  (apply + 
                                    (map 
                                      (lambda (subsubacct)
                                        (accounts-total (xaccAccountGetSplitList subsubacct) 0 subacct))
                                    (gnc-account-get-descendants acct)))
                                  0)) 
                              accounts)))
                        (filter 
                          (lambda(item) 
                            (and 
                              (<= (gnc-account-get-current-depth item) 3) 
                              (or 
                            (null? (gnc-account-get-descendants item)) 
                            (= (gnc-account-get-current-depth item) 3)))) 
                      (filtered-accounts accounts '() "Expenditure"))))))))
          (gnc:html-table-set-style! expendituretable "table"
                                     'attribute (list "style" "width:100%; max-width:120em")
                                     'attribute (list "cellpadding" "0"))
          (gnc:html-document-add-object!
            document expendituretable)
          
          (gnc:html-document-add-object!
            document
            (gnc:make-html-text 
              (gnc:html-markup-h1
                (G_ "Balances and Difference"))))
          (gnc:html-table-append-column!
            table
            (list
              (gnc:make-html-table-cell (G_ " "))
              (gnc:make-html-table-cell 
                (format #f 
                  (G_ "Opening Balance ~a") 
                  (gnc-print-time64 start-date "%Y" )))
              (gnc:make-html-table-cell 
                (format #f
                  (G_ "Closing Balance ~a")
                  (gnc-print-time64 end-date "%Y")))
              (gnc:make-html-table-cell (G_ "Difference in Year"))))
          (map
            (lambda(acct)
              (gnc:html-table-append-column! 
                table
                (list 
                  (gnc:make-html-table-cell (gnc-account-get-full-name acct))
                  (gnc:make-html-table-cell (xaccPrintAmount (balanceincdesc acct start-date) (gnc-default-print-info #f)))
                  (gnc:make-html-table-cell (xaccPrintAmount (balanceincdesc acct end-date) (gnc-default-print-info #f)))
                  (gnc:make-html-table-cell (xaccPrintAmount (- (balanceincdesc acct end-date) (balanceincdesc acct start-date)) (gnc-default-print-info #f))))))
            accounts)
          (gnc:html-table-set-style! table "table"
                                     'attribute (list "style" "width:100%; max-width:120em")
                                     'attribute (list "cellpadding" "0"))
          (gnc:html-document-add-object!
            document table)
          ;;start date is not inclusive of that date, so the previous or next day needs selected to be accurate
          ;; also getting income negative and expenditure positive
          document)))

;; Here we define the actual report with gnc:define-report
(gnc:define-report
 'version 1
 ;; The name of this report. This will be used, among other things,
 ;; for making its menu item in the main menu. You need to use the
 ;; untranslated value here!
 'name (N_ "Annual Accounts")
 ;; The GUID for this report. This string should be unique, set once
 ;; and left alone forever after that. In theory, you could use any
 ;; unique string, even a meaningful one (!) but its probably best to
 ;; use a true uuid. Get them from `uuidgen | sed -e s/-//g` and paste
 ;; the results in here. You must make a new guid for each report!
 'report-guid "db50ba0dee01490c978072a4be198a9c"
 'menu-tip (N_ "Annual Account formatted for Charity Commission Return")
 ;; A path describing where to put the report in the menu system.
 'menu-path (list gnc:menuname-example)
 ;; The options generator function defined above.
 'options-generator options-generator
 ;; The rendering function defined above.
 'renderer annual-accounts-renderer)
