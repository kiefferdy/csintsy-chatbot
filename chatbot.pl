/*
CSINTSY MCO 2: Chat Bot
*/

% Import libraries
:- use_module(library(lists)).

/*
FUNCTIONS AND UTILITIES
*/

% Declares the global lists of user symptoms and symptoms the user does not have
:- dynamic user_symptoms/1.
user_symptoms([]).

:- dynamic rejected_symptoms/1.
rejected_symptoms([]).

:- dynamic user_info/1.
user_info([]).

:- dynamic rejected_info/1.
rejected_info([]).

% Predicates to add elements to the lists
add_to_user_symptoms(Element) :-
    user_symptoms(List),
    append(List, [Element], NewList),   % add the new element to the end of the old list
    retract(user_symptoms(List)),       % remove the old list from memory
    assert(user_symptoms(NewList)).     % assert the new list as the global list

add_to_rejected_symptoms(Element) :-
    rejected_symptoms(List),
    append(List, [Element], NewList),   % add the new element to the end of the old list
    retract(rejected_symptoms(List)),   % remove the old list from memory
    assert(rejected_symptoms(NewList)). % assert the new list as the global list

add_to_user_info(Element) :-
    user_info(List),
    append(List, [Element], NewList),   % add the new element to the end of the old list
    retract(user_info(List)),           % remove the old list from memory
    assert(user_info(NewList)).         % assert the new list as the global list

add_to_rejected_info(Element) :-
    rejected_info(List),
    append(List, [Element], NewList),   % add the new element to the end of the old list
    retract(rejected_info(List)),       % remove the old list from memory
    assert(rejected_info(NewList)).     % assert the new list as the global list

% Count the number of matching elements in two lists
count_matching_items(List1, List2, Count) :-
    findall(Item, (member(Item, List1), member(Item, List2)), MatchingItems),
    length(MatchingItems, Count).

% Capitalize the first letter of a string for grammatical purposes
capitalize_first_letter(String, Capitalized) :-
    sub_atom(String, 0, 1, _, FirstChar),
    upcase_atom(FirstChar, CapitalizedFirstChar),
    sub_atom(String, 1, _, 0, RestOfString),
    atom_concat(CapitalizedFirstChar, RestOfString, Capitalized).

/*
DEFINITIONS
*/

% Defines symptoms (preferably shuffled)
symptom(cough).
symptom(fever).
symptom(chest_pain).
symptom(runny_nose).
symptom(sore_throat).

% Defines diseases and their symptoms
disease(pneumonia, [cough, fever, chest_pain]).
disease(covid, [runny_nose, sore_throat]).

% Defines diagnostic tests or other information associated with the illness
question(old, 'Are you old?').
question(smoker, 'Do you smoke?').
question(low_oxygen, 'Measure your blood oxygen. Is it low?').
question(antigen_positive, 'Perform a rapid antigen test. Is the result positive?').

% Defines which information can be used to confirm which disease
confirm(pneumonia, [old, smoker, low_oxygen]).
confirm(covid, [antigen_positive]).

/*
RULES
*/

% Disregards diseases that do not match user symptoms
disregarded(Disease) :-
    user_symptoms(Symptoms),
    disease(Disease, DiseaseSymptoms),
    not(subset(Symptoms, DiseaseSymptoms)).

% Symptoms marked as available and can be asked
available(Symptom) :-
    symptom(Symptom),
    disease(Disease, DiseaseSymptoms),
    not(disregarded(Disease)),
    user_symptoms(Symptoms),
    rejected_symptoms(RejectedSymptoms),
    not(subset([Symptom], Symptoms)),
    not(subset([Symptom], RejectedSymptoms)),
    subset([Symptom], DiseaseSymptoms).

% Probable diseases for the user along with the number of matching symptoms
probable_diseases(Disease, N, M) :-
    disease(Disease, DiseaseSymptoms),
    user_symptoms(Symptoms),
    subset(Symptoms, DiseaseSymptoms),
    length(Symptoms, N),
    length(DiseaseSymptoms, M).

% Check what information needs to be asked to verify likelihood of initial diagnosis
to_ask(Disease, Information) :-
    question(Information, _),
    confirm(Disease, Confirmations),
    subset([Information], Confirmations),
    user_info(UserInfo),
    rejected_info(RejectedInfo),
    not(subset([Information], UserInfo)),
    not(subset([Information], RejectedInfo)).

/*
MAIN
*/

% Ask questions and determine diagnosis
diagnose :-
    % Checks for matching symptoms
    (
        available(X),
        write('Do you have the following symptom: '), write(X), write('? Answer yes/no: '), nl,
        read(Response),
        (Response == 'yes' -> add_to_user_symptoms(X); add_to_rejected_symptoms(X)),
        diagnose
        ;
        probable_diseases(Y, N, M),
        write('Based on your symptoms, you may have '), write(Y), write('.'), nl,
        write('You are experiencing '), write(N), write(' out of '), write(M), write(' of its possible symptoms.'), nl, nl,
        write('Let\'s try to confirm your disease...'), nl,
        verify(Y)
    )
    ;
    % Backtracks from verify/1 and determines the accuracy of the diagnosis
    (
        user_info(Information),
        confirm(Y, DiseaseInfo),
        count_matching_items(Information, DiseaseInfo, H),
        length(DiseaseInfo, I),
        H >= 1, H < I,
        capitalize_first_letter(Y, YC),
        write(YC), write(' is likely your disease. Please consult a doctor.')
        ;
        user_info(Information),
        confirm(Y, DiseaseInfo),
        count_matching_items(Information, DiseaseInfo, H),
        length(DiseaseInfo, I),
        H =:= I,
        capitalize_first_letter(Y, YC),
        write(YC), write(' is indeed most likely your disease. Please consult a doctor.')
        ;
        user_info(Information),
        confirm(Y, DiseaseInfo),
        count_matching_items(Information, DiseaseInfo, H),
        H =:= 0,
        capitalize_first_letter(Y, YC),
        write(YC), write(' may not actually be your disease. Please consult a doctor.')
    )
    ;
    % If no diagnosis is found
    write('Sorry, we could not determine a diagnosis based on your symptoms. Please consult a doctor.'), nl.

% Check the likelihood of the initial diagnosis
verify(Disease) :-
    to_ask(Disease, Information),
    question(Information, Q),
    write(Q), write(' Answer yes/no: '), nl,
    read(Response),
    (Response == 'yes' -> add_to_user_info(Information); add_to_rejected_info(Information)),
    verify(Disease)
    ;
    fail.

% Runs the program upon consultation by calling diagnose/0
:- diagnose.