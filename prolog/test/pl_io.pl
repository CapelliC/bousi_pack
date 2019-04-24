%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Tests for I/O predicates

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%
% List of test suites
%

test_suites([test_open_close, test_read_1, test_read_2, test_write_1,
             test_write_2, test_nl_1, test_nl_2, test_get_char_1,
             test_get_char_2, test_put_char_1, test_put_char_2]).


%
% open/3 and close/1 tests
% - ISO -
%

% These tests have been created manually because the 2nd draft of the
% ISO Prolog Standard have very few examples for these predicates

test_open_close_1 :- tmp_file('test', File),
                     tell(File), told,
                     open(File, read, Stream),
                     close(Stream),
                     delete_file(File).
test_open_close_2 :- tmp_file('test', File),
                     open(File, write, Stream),
                     close(Stream),
                     delete_file(File).
test_open_close_3(Term) :- tmp_file('test', File),
                           open(File, write, StreamW),
                           write(StreamW, 'sample.'),
                           close(StreamW),
                           open(File, read, StreamR),
                           read(StreamR, Term),
                           close(StreamR),
                           delete_file(File).
test_open_close_4 :- open('file', unknown, Stream).
test_open_close_5 :- open('non-existent', read, Stream).

throws_exception(test_open_close_4).
throws_exception(test_open_close_5).


%
% read/1 tests
% - ISO -
%

% Note that these tests have additional code in order to create
% and use temporary files

test_read_1_1(T) :- create_temp_file('term1. term2.', File),
                    see(File),
                    read(T),
                    seen,
                    delete_file(File).
test_read_1_2 :- create_temp_file('term1. term2.', File),
                 see(File),
                 read(term1),
                 seen,
                 delete_file(File).
test_read_1_3(T) :- create_temp_file('3.1. term2.', File),
                    see(File),
                    read(T),
                    seen,
                    delete_file(File).
test_read_1_4 :- create_temp_file('3.1. term2.', File),
                 see(File),
                 (
                    read(4.1),
                    seen,
                    delete_file(File)
                 ;
                    seen,
                    delete_file(File),
                    fail
                 ).
test_read_1_5(T) :- create_temp_file('foo 123. term2.', File),
                    see(File),
                    catch(
                          (read(T)),
                          Error,
                          (seen, delete_file(File), throw(Error))
                    ),
                    seen,
                    delete_file(File).
test_read_1_6(T) :- create_temp_file('3.1', File),
                    see(File),
                    catch(
                          (read(T)),
                          Error,
                          (seen, delete_file(File), throw(Error))
                    ),
                    seen,
                    delete_file(File).

throws_exception(test_read_1_5).
throws_exception(test_read_1_6).


%
% read/2 tests
% - ISO -
%

% There're no examples for this predicate in the 2nd draft of ISO Prolog
% Standard; the following tests have been created from the read/1 examples

test_read_2_1(T) :- create_temp_file('term1. term2.', File),
                    open(File, read, Stream),
                    read(Stream, T),
                    close(Stream),
                    delete_file(File).
test_read_2_2 :- create_temp_file('term1. term2.', File),
                 open(File, read, Stream),
                 read(Stream, term1),
                 close(Stream),
                 delete_file(File).
test_read_2_3(T) :- create_temp_file('3.1. term2.', File),
                    open(File, read, Stream),
                    read(Stream, T),
                    close(Stream),
                    delete_file(File).
test_read_2_4 :- create_temp_file('3.1. term2.', File),
                 open(File, read, Stream),
                 (
                    read(Stream, 4.1),
                    close(Stream),
                    delete_file(File)
                 ;
                    close(Stream),
                    delete_file(File),
                    fail
                 ).
test_read_2_5(T) :- create_temp_file('foo 123. term2.', File),
                    open(File, read, Stream),
                    catch(
                          (read(Stream, T)),
                          Error,
                          (close(Stream), delete_file(File), throw(Error))
                    ),
                    close(Stream),
                    delete_file(File).
test_read_2_6(T) :- create_temp_file('3.1', File),
                    open(File, read, Stream),
                    catch(
                          (read(Stream, T)),
                          Error,
                          (close(Stream), delete_file(File), throw(Error))
                    ),
                    close(Stream),
                    delete_file(File).

throws_exception(test_read_2_5).
throws_exception(test_read_2_6).


%
% write/1 tests
% - ISO -
%

% There're no examples for this predicate in the 2nd draft of ISO Prolog
% Standard; the following tests have been created from the write/2 examples

test_write_1_1 :- write([1, 2, 3]).
test_write_1_2 :- write(1 < 2).
test_write_1_3 :- write('1<2').
test_write_1_4 :- write('$VAR'(0) < '$VAR'(1)).


%
% write/2 tests
% - ISO -
%

% Note that these tests have additional code in order to create and
% use temporary files, and verify the behavior of the write/2 predicate

test_write_2_1(Codes) :- tmp_file('test', File),
                         open(File, write, Stream),
                         write(Stream, [1, 2, 3]),
                         close(Stream),
                         read_file_to_codes(File, Codes, []),
                         delete_file(File).
test_write_2_2(Codes) :- tmp_file('test', File),
                         open(File, write, Stream),
                         write(Stream, 1 < 2),
                         close(Stream),
                         read_file_to_codes(File, Codes, []),
                         delete_file(File).
test_write_2_3(Codes) :- tmp_file('test', File),
                         open(File, write, Stream),
                         write(Stream, '1<2'),
                         close(Stream),
                         read_file_to_codes(File, Codes, []),
                         delete_file(File).
test_write_2_4(Codes) :- tmp_file('test', File),
                         open(File, write, Stream),
                         write(Stream, '$VAR'(0) < '$VAR'(1)),
                         close(Stream),
                         read_file_to_codes(File, Codes, []),
                         delete_file(File).


%
% nl/0 tests
% - ISO -
%

test_nl_1_1 :- nl, put_char(a).


%
% nl/1 tests
% - ISO -
%

% Note that these tests have additional code in order to create and
% use temporary files, and verify the behavior of the nl/1 predicate

test_nl_2_1(Codes) :- tmp_file('test', File),
                      open(File, write, Stream),
                      nl(Stream), put_char(Stream, a),
                      close(Stream),
                      read_file_to_codes(File, Codes, []),
                      delete_file(File).
test_nl_2_2(Str) :- nl(Str).
test_nl_2_3 :- nl([my_file]).

throws_exception(test_nl_2_2).
throws_exception(test_nl_2_3).


%
% get_char/1 tests
% - ISO -
%

% Note that these tests have additional code in order to create
% and use temporary files

test_get_char_1_1(Char) :- create_temp_file('qwerty', File),
                           see(File),
                           get_char(Char),
                           seen,
                           delete_file(File).


%
% get_char/2 tests
% - ISO -
%

% Note that these tests have additional code in order to create
% and use temporary files

test_get_char_2_1(Char) :- create_temp_file('qwerty', File),
                           open(File, read, Stream),
                           get_char(Stream, Char),
                           close(Stream),
                           delete_file(File).
test_get_char_2_2(Char) :- create_temp_file('\'qwerty\'', File),
                           open(File, read, Stream),
                           get_char(Stream, Char),
                           close(Stream),
                           delete_file(File).
test_get_char_2_3 :- create_temp_file('\13\10\newline', File),
                     open(File, read, Stream),
                     get_char(Stream, '\13\'),
                     close(Stream),
                     delete_file(File).
test_get_char_2_4(Char) :- create_temp_file('', File),
                           open(File, read, Stream),
                           get_char(Stream, Char),
                           close(Stream),
                           delete_file(File).
test_get_char_2_5(X) :- get_char(user_output, X).
test_get_char_2_6 :- create_temp_file('qwerty', File),
                     open(File, read, Stream),
                     (
                        get_char(Stream, p),
                        close(Stream),
                        delete_file(File)
                     ;
                        close(Stream),
                        delete_file(File),
                        fail
                     ).

throws_exception(test_get_char_2_5).


%
% put_char/1 tests
% - ISO -
%

test_put_char_1_1 :- put_char(t).


%
% put_char/2 tests
% - ISO -
%

% Note that these tests have additional code in order to create and use
% temporary files, and verify the behavior of the put_char/2 predicate 

test_put_char_2_1(Codes) :- tmp_file('test', File),
                            open(File, write, Stream),
                            put_char(Stream, t),
                            close(Stream),
                            read_file_to_codes(File, Codes, []),
                            delete_file(File).
test_put_char_2_2(C) :- put_char(my_file, C).
test_put_char_2_3(Stream, C) :- put_char(Stream, C).
test_put_char_2_4(Codes) :- tmp_file('test', File),
                            open(File, write, Stream),
                            put_char(Stream, 'A'),
                            close(Stream),
                            read_file_to_codes(File, Codes, []),
                            delete_file(File).

throws_exception(test_put_char_2_2).
throws_exception(test_put_char_2_3).


%
% Helper predicates
%

create_temp_file(Text, File) :- tmp_file('test', File),
                                open(File, write, Stream),
                                write(Stream, Text),
                                close(Stream).

