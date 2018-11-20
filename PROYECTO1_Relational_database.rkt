#lang racket

;MAIN WELCOME AND MAIN LOOP
;Function that starts the program it calls the dataBaseManager function where the loop of the program is in.
(define(init)
  (display "Welcome to Relational Database")
  (newline)
  (dataBaseManager (list "TABLAS") (list ) )
  )
  

(define (dataBaseManager mainList listProcedures) ;it is need to read the command and assign the value to the next funtion
  (dataBaseManagerAux mainList (getCommand) listProcedures)
  )

(define (dataBaseManagerAux mainList command listProcedures) ; it is the auxiliary function of the main loop
                                                             ; to manage all the process
   (cond
    ((equal? (symbol->string(car command)) "end") (display "Thanks for using Relational Database"))
    (( or (equal? (symbol->string(car command)) "cproc") (equal? (symbol->string(car command)) "createProcedure"))
     (dataBaseManager mainList (append listProcedures (list (cdr command)))))
    ((equal? (symbol->string(car command)) "eval") (eval listProcedures (cdr command) listProcedures mainList))
    (#t (dataBaseManager (funcManager mainList command listProcedures) listProcedures))
  )
  )

(define(eval listProcedures commandInfo listProceduresAux mainList) ;eval function use to call the stored procedures
  (display listProcedures)
  (cond
    ((null? listProcedures)(display "Procedure not found") (dataBaseManager mainList listProcedures))
    ((equal? (symbol->string(caar listProcedures)) (symbol->string(car commandInfo)))
     (evalAux listProcedures commandInfo listProceduresAux mainList))
    (else (eval (cdr listProcedures) commandInfo listProceduresAux mainList))
  )
  )

(define(evalAux listP commandInfo listPAux mainList)  ;Auxiliary function of eval
  (cond
    ((equal? (length (cadr(car listP))) (length (cdr commandInfo))) (evalAuxAux listP commandInfo listPAux mainList))
    (else (display "ERROR DATA INPUT")(dataBaseManager mainList listPAux))
    )
  )

(define(evalAuxAux listP commandInfo listPAux mainList)
  (dataBaseManager (funcManager mainList (flatten (list (list  (caddar listP)) (list (car (cdddar listP))) (cdr commandInfo) ) ) listPAux) listPAux)
  
  
  )

(define (getCommand) ;function to get the information from the user, it is like a command line
  (display "Command:")
  (newline)
  (read (current-input-port))
  )
(define (symbol_String commandList stringList)(cond[(null? commandList) stringList] ;chances to string the elements of a list
                                                   ((number? (car commandList)) (symbol_String (cdr commandList)(append stringList (list(number->string (car commandList))))))
                                                  
                                                   [else (symbol_String (cdr commandList)(append stringList (list(symbol->string (car commandList)))))]))



(define (funcManager mainList command listProcedures) ;it manages the call of the functions according to the command input from the user
  (cond
    ((equal? (symbol->string(car command)) "showall") (showAll (cdr mainList)) mainList)
    ((equal? (length command) 1) (display "Error, the program needs more information")(newline) mainList)
    (( or (equal? (symbol->string(car command)) "addtable") (equal? (symbol->string(car command)) "addt") )  (addTable mainList (list (symbol->string(car (cdr command))) (symbol_String (cdr (cdr command)) '()) ) ))
    (( or (equal? (symbol->string(car command)) "insert") (equal? (symbol->string(car command)) "ins") )  (Insert mainList (car (list (symbol->string(car (cdr command))) (symbol_String (cdr (cdr command)) '()) )) (car (cdr (list (symbol->string(car (cdr command))) (symbol_String (cdr (cdr command)) '()) ))))) 
    (( or (equal? (symbol->string(car command)) "remover") (equal? (symbol->string(car command)) "rr") )  (deleteRow mainList (car (list (symbol->string(car (cdr command))) (symbol_String (cdr (cdr command)) '()) )) (car (cdr (list (symbol->string(car (cdr command))) (symbol_String (cdr (cdr command)) '()) )))))
    (( or (equal? (symbol->string(car command)) "deltable") (equal? (symbol->string(car command)) "dt") )  (deleteTable mainList (car(list (symbol->string(car (cdr command))))) ))
    (( or (equal? (symbol->string(car command)) "query"))(query (list (symbol->string(car (cdr command))) (symbol_String (cdr (cdr command)) '()) ) mainList) mainList)
    (( or (equal? (symbol->string(car command)) "update") (equal? (symbol->string(car command)) "ud")) (Update mainList (car (list (symbol->string(car (cdr command))) (symbol_String (cdr (cdr command)) '()) )) (caadr (list (symbol->string(car (cdr command))) (symbol_String (cdr (cdr command)) '()) )) (symbol_String(getP (cdr command )) '()) (symbol_String(getI (cdr command )) '() )))
    (#t(display "Error instruction not valid")(newline))
))
;*************************************************************************************************************************
;UPDATE

(define (getP listP) ; Auxiliary function
  
  (getPa (cddr listP))
  )


(define (getI listI) ;Auxiliary function
  
  (getIm (cddr listI))
  )
(define (getIm listCross) ;function to get the odd elements of a list according to the order 
  
  (cond
    [(null?  listCross) '()]
    [(and (null? (cdr listCross)) (not (null? listCross))) (list(car listCross))]
    [(null? (cdr listCross)) '()]
    [else (flatten (list (car listCross) (getIm (cddr listCross)) ) )]
    
    )
  )

(define (getPa listCross) ;function to get the pair elements of a list according to the order
  
  (cond
    [(null? listCross) '()]
    [(null?  ( cdr listCross)) '()]
    
    [else (flatten (list (cadr listCross) (getPa (cddr listCross)) ) )]
    
    )
  )




; Function update to chance the values of the data inside the tables

(define (Update master tableName data dataList columnNames)
  
  (cond[(null? master)(display "No table Found")(master)]
                                                               
                                                               
                                          [(string? (car master)) (cons (car master)  (Update (cdr master) tableName data dataList columnNames) ) ]
                                          [(list? (car master)) (UpdateAux1 master tableName data dataList columnNames) ]))
;First auxiliary function of the update
(define (UpdateAux1 master tableName data dataList columnNames)(cond[(string=? (caar master) tableName)(cons (cons(caar master ) (UpdateAux2 master tableName data dataList columnNames))  (cdr master)) ]
                                          [#t (append (car master) (Update (cdr master) tableName data) )]))
;Second auxiliary function of the update
(define (UpdateAux2 master tableName data dataList columnNames)  (cond [(> (findRowNumber2 (cadadr (car master)) data 0 (length (cadadr (car master)))) 0) ( UpdateRowTable (cdar master)  (findRowNumber2 (cadadr (car master)) data 0 (length (cadadr (car master)))) dataList columnNames ) ]
                                                 [#t (car master) ]))

;Updates the row of the tables 
(define(UpdateRowTable table index data columnNames ) (map (lambda (columns index info )(cond[(equal? info "L") (list(car columns) (cadr columns))]
                                                                                 [#t  ( list (car columns) (flatten(list (drop-right (cadr columns) index ) info (take-right (cadr columns) (- index 1)))))]
                                                                                 )
                                                 )
                                              table (build-list (length  table) (lambda (x) (values index))) (flatten(SortInfo columnNames columnNames table data data )) ) )
;organize information form the tables
(define (SortInfo columnNames columnNames2 columns data data2)(cond[(null? columnNames) (append '("L") (SortInfo columnNames2 columnNames2 (cdr columns) data2 data2))]
                                                       [(null? columns) columns ]
                                                       [(equal? (caar columns) (car columnNames))  (list (car data) (SortInfo (remove (car columnNames) columnNames2) columnNames2  (cdr columns)(remove (car data) data2) data2)) ]                                                       
                                                       [#t  (list (SortInfo(cdr columnNames) columnNames2  columns  (cdr data) data2) )   ]
))

;**************************************************************************************************************************
;ADD TABLES
(define (firstRowDel Input Output)(cond [(null? Input) Output "c"]
                                         [else (firstRowDel (cdr Input) (append Output(list (cdr(car Input))) ))]))
(define (listColCreate listCols listOutput)(cond[(null? listCols) listOutput]                                              
                                                  [(list? listCols)  (listColCreate (cdr listCols) (append  listOutput (list(list (car listCols) (list)))))]))
(define (createTable listCols)(cond[(null? listCols)]
                                    [(string?(car listCols)) (list(append (list (car listCols)) (listColCreate (car(cdr listCols)) '()))) ]))
;Creates a new list with two elements, the first is a string with the name of the list, it would be the name of the column,
;and the other element is a list, where the data of the column is in, then the list is append to the main list of
;the program.
(define (addTable mainList tableInfo)
  (cond
    ((equal? (cdr tableInfo) '(()))  (display "Please add the table name and the attributes")(newline) mainList)
    (#t (append mainList (createTable tableInfo)))
    )
  )
;**************************************************************************************************************************
;INSERT DATA

; All functions are auxiliary  and the main function to insert is Insert
;Insert function: it's function is to add the new data from the user to store in the data base,
;it has validations and the information is stored in the second element from the table created with the
;function addTable.
(define (InsertCol col strings)(list (car col) (cons  strings (cadr col))))
(define (InsertarenCols cols data)(cond[(not(= (length data)(length  cols))) (display "Error less data than columns \n")cols ]
                                       [(null? (cdr cols)) (list(InsertCol (car cols) (car data)))]
                                        [#t  (cons(InsertCol  (car cols) (car data)) (InsertarenCols (cdr cols) (cdr data))) ] ))
(define (InsertAux1 master tableName data)(cond[(null? master )(display "Table not found") master]
                                              [(string? (car master)) (cons (car master)  (InsertAux1 (cdr master) tableName data) ) ]
                                              [(list? (car master)) (InsertAux2 master tableName data) ]))

(define (InsertAux2 master tableName data)(cond[(string=? (caar master) tableName)(cons (cons(caar master ) (InsertarenCols (cdar master) data )) (cdr master) )]
                                          [#t (append (list(car master)) (InsertAux1 (cdr master) tableName data) )]))

;Check the the information to before write it into a tables
(define (checkData tables tableName  data) (cond [(checkNumData (getColumns tables tableName) data) #t]
                                                [#t (display "Error while inserting please check the data")  #f]) )

;check the number of columns
(define (checkNumData data columns)(cond [(or (not (list? data))(not (list? columns))) #f]
                                      [(= (length data) (length columns)) #t]
                                      [#t #f]))
(define (getColumns tables tableName) (cond[(null? tables) (display "No table with that name \n")tables]
                                           [(list? (car tables)) (getColumnsAux tables tableName)]
                                           [(string? (car tables))(getColumns (cdr tables) tableName)]))
(define (getColumnsAux tables tableName) (cond[(string=? tableName (caar tables))(cdar tables)]
                                              [else (getColumns (cdr tables) tableName)]))

(define(chkpk columns str )(cond [(null? columns) columns]
                                 [#t (chkrow (cadar columns) str )] ))

(define (chkrow inData str) (cond[(null? inData) #t]
                                 [(equal? (car inData) str) (display "Primary Key constraint violated") (newline)#f ]
                                 [#t (chkrow (cdr inData) str )]))

(define (Insert master tableName data ) (cond[(chkpk (getColumns master tableName) (car data)) (InsertAux1 master tableName data) ]
                                            [#t master]) )
                                        
;**************************************************************************************************************************
;DELETE RECORDS

;All functions are auxiliary except deleteRow
;it is used to remove the corresponding row form the corresponding table, according to the information provided
;by the user. It cross the list of the table  to find the right row, also has validations and uses other auxiliary
;functions.
(define (findRowNumber inData str contador largo) (cond[(null? inData) (display "Row not found\n") (values -1)]
                                 [(equal?  (car inData) (car str)) (newline) (- largo contador) ]
                                 [#t (findRowNumber (cdr inData) str (+ contador 1) largo)]))
(define (findRowNumber2 inData str contador largo) (cond[(null? inData) (display "Row not found\n") (values -1)]
                                 [(equal?  (car inData)  str) (newline) (- largo contador) ]
                                 [#t (findRowNumber (cdr inData) str (+ contador 1) largo)]))

(define(deleteRowTable table index) (map (lambda (columns index )
       ( list (car columns) (append (drop-right (cadr columns) index ) (take-right (cadr columns) (- index 1)))))
        table (build-list (length  table) (lambda (x) (values index))) ) )

(define (deleteRow master tableName data)(cond[(null? master )(display "Table not found") master]
                                             [(string? (car master)) (cons (car master)  (deleteRow (cdr master) tableName data) ) ]
                                             [(list? (car master)) (deleteAux2 master tableName data) ]))

(define (deleteAux2 master tableName data)(cond[(string=? (caar master) tableName)(cons (cons(caar master ) (deleteAux1 master tableName data))  (cdr master)) ]
                                          [#t (append (list(car master)) (deleteRow (cdr master) tableName data) )]))

(define (deleteAux1 master tableName data)  (cond [(> (findRowNumber (cadadr (car master)) data 0 (length (cadadr (car master)))) 0)( deleteRowTable (cdar master)  (findRowNumber (cadadr (car master)) data 0 (length (cadadr (car master)))) )]
                                                 [#t (cdar master) ]))
;***************************************************************************************************************************
;QUERY
;All functions are auxiliry except query, it is the main function,
;it has the job to print specified information according to the user
(define (query toQuery mainList)
  (cond
    ((equal? (searchListQuery (cdr mainList) (car toQuery)) #t)(display "ERROR, table doesn't exist") mainList)
    ((equal? (cdr toQuery) '(()))(queryAllColumns (car toQuery) (cdr mainList)))
    (else (searchQueryColumns (car toQuery) (cdr mainList) toQuery))
   )
  )

; Find the columns to check them exist
(define(searchQueryColumns tableName table toQuery)
  (cond
  ((equal? (caar table) tableName)(queryColumns (list tableName) (cdr(car table)) (car(cdr toQuery))))
  (else (searchQueryColumns tableName (cdr table) toQuery))
  ))


(define (queryColumns listShow table columns)
  
  (cond
    ((equal? columns null) (showAll (list listShow)))
    (else (queryColumns (prepareQuery (car columns) table listShow) table (cdr columns)))
    )
  )
;makes the search of the columns
(define (prepareQuery columnName table listShow)
  (cond
    ((equal? table null)listShow)
    ((equal? columnName (caar table))(append listShow (list (car table))))
    (else (prepareQuery columnName (cdr table) listShow))
    )
  )
(define(queryAllColumns tableName table)
  (cond
  ((equal? (caar table) tableName)(showAll (list (car table))))
  (else (queryAllColumns tableName (cdr table)))
  ))
; Find the tables that the user wants to get
(define (searchListQuery mainList tableName) ; return true if the list is in the mainlist
  (cond
  ((equal?  mainList '()) #t)
  ((equal? (caar mainList) tableName) #f)
  (else (searchListQuery (cdr mainList) tableName))
  )
  )
;***********************************************************************************************************************
;SHOWALL
;It is used to show all the information inside the all the tables of the program, it prints in the console clear and
;organized

(define(showAll tables)
  (cond
    ((equal? tables null) (display "Tables printed")(newline))
    (else (printf "table: ")(display (caar tables))(newline)(showAllAux (cdr(car tables)) (cdr(car tables)))(newline) (showAll (cdr tables)))
  )
  )

(define (showAllAux listTables listTablesAux) ; it prints the tables like a tables are
  (cond
    ((equal? listTables null)(newline)(display "----------------------------------------------------------------------------")(newline) (listData (createDataList listTablesAux (list )) (createDataList listTablesAux (list )) ))
    (else (display (caar listTables))(display "           ") (showAllAux (cdr listTables) listTablesAux) )
    )
  )
;Creates a data list that are the columns of the tables to print
(define (createDataList listData newList)
  (cond
    ((equal? listData null)newList)
    (else (createDataList (cdr listData) (append newList (cdr(car listData)))))
    )
  )

(define (listData listOnlyData listAux)
  (cond
    ((equal? (car listOnlyData) '()) )
    (else (printRow listOnlyData)(newline)(listData (deleteFirsts listOnlyData (list )) listAux) )
  )
  )
;Prints the rows of the tables
(define (printRow listAux)
  
  (cond
    ((equal? listAux null) )
    (else (display (caar listAux))(display "             ")(printRow (cdr listAux)))
    )
  )

(define (deleteFirsts listA newList)
  (cond
    ((equal? listA null) newList)
    (else (deleteFirsts (cdr listA) (append newList (list (cdr(car listA))))))

    )

  )
;**************************************************************************************************************************
;DELETE TABLES

;Main function is deleteTable and deletes or eliminates tables that are empty as the requirements of the project specifies.
(define (deleteTable mainList tableToDelete)
  (cond
    ((equal? (searchList (cdr mainList) tableToDelete) #t) (display "Error, the table doesn't exist") mainList)
    ((equal? (tableFull (cdr mainList) tableToDelete ) #t)(display "Error, Only the empty tables can be removed") mainList)
    (#t (removeTableFinally mainList tableToDelete mainList))
    
   )
  )
; the last function called to delete the tables
(define (removeTableFinally mainList tableToDelete mainListAux)
  (cond
    ((equal? (caar (cdr mainList)) tableToDelete) (remv (car (cdr mainList)) mainListAux))
    (else (removeTableFinally (cdr mainList) tableToDelete mainListAux))
  )
  )

(define (searchList table tableToDelete) ; return true if the list is in the mainlist
  (cond
  ((equal?  table '()) #t)
  ((equal? (caar table) tableToDelete) #f)
  (else (searchList (cdr table) tableToDelete))
  )
  )
; checks if the tables are full or not
(define(tableFull table tableToDelete)
  (cond
    ((equal?  table '()) #t)
    ((equal? (caar table) tableToDelete) (tableFullAux (cdr(car table))))
    (else (tableFull (cdr table) tableToDelete))
  )
  )
; auxiliary function of tableFull
(define (tableFullAux tablesCols)
  (cond
   ((equal? tablesCols '()) #f)
   ((equal? (elementInNull (car(cdr(car tablesCols)))) #t) (tableFullAux (cdr tablesCols)))
    (else #t)
  
  )
  )

(define(elementInNull listToCheck)
  (cond
    ((equal? listToCheck '()) #t)
    ((equal? (car listToCheck) "null") (elementInNull (cdr listToCheck)) )
    (else #f)
   
  )
  )

(init)
