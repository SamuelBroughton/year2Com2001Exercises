
-- PROBLEM 1 (a)

data Book = Book { bookId :: Int }
data Reader = Reader { readerID :: Int }
type Loan = (Book, Reader)

{- Functions needed and their potential error condidions:
 newLib     : no error conditions
 getBooks   : no error conditions 
 getReaders : no error conditions
 getLoans   : no error conditions
 addBook    : adding a book that already exists in the Library
 addReader  : adding a reader that already has been registered
 addLoan    : loaning a book that has already been loaned;
              loaning a book to an unregistered reader;
              loaning a book that doesnt exist
 delBook    : deleting a book that does not exist
 delReader  : deleting a reader that is not registered
 delLoan    : cancelling a loan that has not been placed
-}

-- PROBLEM 1 (b)

{- Syntax of the ADT, use of Maybe due to potential error conditions
 newLib     :: Library
 getBooks   :: Library -> [Books]
 getReaders :: Library -> [Readers]
 getLoans   :: Library -> [Loans]
 addBook    :: Library -> Book -> (Library U ErrBook)
 addReader  :: Library -> Reader -> (Library U ErrReader)
 addLoan    :: Library -> Book -> Reader -> (Library U ErrBook U ErrReader)
 delBook    :: Library -> Book -> (Library U ErrBook)
 delReader  :: Library -> Reader -> (Library U ErrReader)
 delLoan    :: Library -> Book -> Reader -> (Library U ErrBook U ErrReader)
-}

-- PROBLEM 1 (c)

{- ADT sorts
 Library    : the type being defined
 Book       : existing type representing books; 
              represent lists of books;
              assume empty list i.e. noBooks
 Reader     : existing type representing readers;
              represent lists of readers;
              assume empty list i.e. noReaders
 Loan       : existing type representing loans;
              represent lists of loans;
              assusme empty list i.e. noLoans;
              3 functions defined in relations to Loan;
                fst     :: Loan -> Book
                snd     :: Loan -> Reader
                mkParit :: Book -> Reader -> Loan
              satisfying
                mkPair (fst l) (snd l) = l
                fst (mkPair b r)       = b
                snd (mkPair b r)       = r
 ErrBook    : representing errors relating to type Book;
              error for non existant books errNoSuchBook;
              error for duplicate books errBookExists
 ErrReader  : representing errors relating to type Reader;
              error for non existant readers errNoSuchReader;
              error for duplicate readers errReaderExists
 ErrLoan    : representing errors relating to type Loan;
              error for non existant loans errNoSuchLoan;
              error for duplicate loans errLoanExists
-}

-- PROBLEM 1 (d)

{- Library constructors and semantics
 Constructors : newLib, addBook, addReader, addLoan
 Observers    : getBook, getReader, getLoan
 Mutators     : delBook, delReader, delLoan
 
 4 constructors and 6 non-constructors, so 4 * 6 = 24 rules
-}

-- PROBLEM 1 (e)

{- Semantics for ADT, what each rule means. Applying each non-constructor following each constructor.
 
 newLib=
   getBook newLib   = noBooks
   getReader newLib = noReaders
   getLoan newLib   = noLoans
   delBook          = errNoSuchBook
   delReader        = errNoSuchReader
   delLoan          = errNoSuchLoan

 addBook, addReader and addLoan are the same except for addLoan should check if a Reader or Book exist.
 addBook and addReader should create an errXXXXExists if you try and add a duplicate book or reader.
 Also deleting any book or reader should delete any loans including this book or reader.
 
 addBook (is analogous for addReader and addLoan)=
   getBook (addBook l b)      = {b} U getBooks l
   -- b is added to books already in l
   getReader (addBook l b)    = getReaders l
   -- Reader is unaffected
   getLoan (addBook l b)      = getLoans l
   -- Loan is unaffected
   delBook (addBook l b) b'   = if (b == b') then l
                              else addBook (delBook l b') b
   -- Deleting a book just added will return the origional library
   -- Deleting a different book, delete that book before adding another
   delReader (addBook l b) r'  = delReader l r'
   -- Reader is unaffected
   delLoan (addBook l b) b' r' = delLoan l b' r'
   -- Loan is unaffected 
-}






-- PROBLEM 2 (a)

{- ADT for Deque
 
 SYNTAX
  
  create      :: Deque
  addFront    :: Value -> Deque -> Deque
  addBack     :: Value -> Deque -> Deque
  empty       :: Deque -> Bool
  removeFront :: Deque -> (Deque U ErrDeque)
  removeBack  :: Deque -> (Deque U ErrDeque)
  front       :: Deque -> (Value U ErrValue)
  back        :: Deque -> (Value U ErrValue)
  
 SORTS
 
  Deque    : the type being defined
  Value    : an elements that are stored within the Deque
  Bool     : boolean values. True and False assumed
  ErrDeque : error that occurs when Deque output is expected
  ErrValue : error that occurs when Value output is expected
  
 SEMANTICS
 
  Constructors : create, addFront, addBack
  Observers    : empty, front, back
  Mutators     : removeFront, removeBack
  
  3 constructors and 5 non-constructors, so 3 * 5 = 15 rules
  
  empty=
    empty (create)       = True
    empty (addFront v d) = False
    empty (addBack v d)  = False
    
  front=
    front (create)           = ErrValue
    front (addFront v d)     = v
    front (addBack v create) = v
    front (addBack v d)      = front d
    
  back=
    back (create)            = ErrValue
    back (addFront v create) = v
    back (addFront v d)      = back d
    back (addBack v d)       = v
    
  removeFront=
    removeFront (create)            = ErrDeque
    removeFront (addFront v create) = create
    removeFront (addFront v d)      = d
    removeFront (addBack v create)  = create
    removeFront (addBack v d)       = addBack v (removeFront d)
    
  removeBack=
    removeBack (create)            = ErrDeque
    removeBack (addFront v create) = create
    removeBack (addFront v d)      = addFront v (removeBack d)
    removeBack (addBack v create)  = create
    removeBack (addBack v d)       = d
-}

-- PROBLEM 2 (b)

data Deque a = Create | AddFront a (Deque a) | AddBack a (Deque a)
               deriving (Show)
               
errValue = error "No such value"
errDeque = error "No such Deque"

empty (Create) = True
empty (AddFront v d) = False
empty (AddBack v d) = False

removeFront (Create) = errDeque
removeFront (AddFront _ Create) = Create
removeFront (AddFront _ d) = d
removeFront (AddBack _ Create) = Create
removeFront (AddBack v d) = AddBack v (removeFront d)

removeBack (Create) = errDeque
removeBack (AddFront _ Create) = Create
removeBack (AddFront v d) = AddFront v (removeBack d)
removeBack (AddBack _ Create) = Create
removeBack (AddBack _ d)       = d

front (Create) = errValue
front (AddFront v _) = v
front (AddBack v Create) = v
front (AddBack _ d) = front d

back (Create) = errValue
back (AddFront v Create) = v
back (AddFront _ d) = back d
back (AddBack v _) = v


























