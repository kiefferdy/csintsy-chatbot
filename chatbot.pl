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
symptom(chest_pain).
symptom(runny_nose).
symptom(sore_throat).
symptom(weak).
symptom(appetite_loss).
symptom(fatigue).
symptom(confusion).
symptom(hard_swallow).
symptom(irritable).
symptom(nausea).
symptom(headache).
symptom(bodyache).
symptom(abdominal_pain).
symptom(abdominal_cramps).
symptom(stomach_pain).
symptom(muscle_pain).
symptom(joint_pain).
symptom(rash).
symptom(colds).
symptom(hard_breathe).
symptom(chills).
symptom(diarrhea).
symptom(watery_diarrhea).
symptom(vomiting).
symptom(no_taste).
symptom(thirst).
symptom(leg_cramps).
symptom(rapid_heart).
symptom(loss_skin_elasticity).
symptom(dry_mucous_membrane).
symptom(low_bp).
symptom(bloating).
symptom(bloody_poo).
symptom(pus_poo).
symptom(pain_behind_eyes).
symptom(swollen_glands).
symptom(had_dengue).
symptom(constipation).
symptom(night_sweats).
symptom(blood_cough).
symptom(phlegm).
symptom(weight_loss).
symptom(blisters).
symptom(insomnia).
symptom(anxiety).
symptom(partial_paralysis).
symptom(hyperactivity).
symptom(hallucinations).
symptom(more_saliva).
symptom(weight_loss).
symptom(pink_eye).
symptom(feel_unwell).

% Defines diseases and their symptoms
disease(tuberculosis, [cough, fever, weak, night_sweats, chest_pain, weight_loss, appetite_loss, blood_cough, phlegm]).
disease(typhoid_fever, [fever, weak, stomach_pain, headache, diarrhea, constipation, cough, appetite_loss, rash]).
disease(dengue, [fever, headache, pain_behind_eyes, muscle_pain, joint_pain, nausea, vomiting, swollen_glands, rash]).
disease(flu, [fever, chills, cough, sore_throat, colds, muscle_pain, bodyache, headache, fatigue, vomiting, diarrhea]).
disease(stomach_flu, [fever, appetite_loss, bloating, nausea, vomiting, stomach_pain, abdominal_pain, abdominal_cramps, diarrhea, blood_poo, pus_poo, feel_unwell]).
disease(cholera, [diarrhea, watery_diarrhea, vomiting, thirst, leg_cramps, irritable, loss_skin_elasticity, dry_mucous_membrane]).
disease(rabies, [insomnia, anxiety, confusion, partial_paralysis, hyperactivity, irritable, hallucinations, more_saliva, hard_swallow]).
disease(pneumonia, [fever, cough, chest_pain, phlegm, blood_cough, chills, hard_breathe, appetite_loss, fatigue, nausea, vomiting, confusion]).
disease(covid, [fever, colds, sore_throat, cough, no_taste, no_smell, hard_breathe, chills, headache, chest_pain, pink_eye, nausea, vomiting, diarrhea, rash]).
disease(hand_foot_mouth, [fever, sore_throat, feel_unwell, rash, irritable, appetite_loss]).

% Defines diagnostic tests or other information associated with the illness
question(old, 'Are you old?').
question(young, 'Are you a young adult/adolescent (less than 24 yrs old)?').
question(child, 'Are you a child?').
question(smoker, 'Do you smoke?').
question(had_dengue, 'Have you had dengue before?').

question(low_oxygen, 'Measure your blood oxygen. Is it low?').
question(low_bp, 'Measure your blood pressure. Is it low?').
question(rapid_heart, 'Measure your heart rate. Is it high?').

question(antigen_positive, 'Perform a rapid antigen test. Is the result positive?').
question(animal_wound, 'Have you been bitten or scratched by an animal?').
question(exposure, 'Have you been exposed to sick people?').
question(blister, 'Examine mouth. Are there any painful, red, blister-like lesions?').
question(pink_eye, 'Examine eye. Is there swelling and/or redness?').

% Defines which information can be used to confirm which disease
confirm(tuberculosis, [child, young, exposure]).
confirm(typhoid_fever, []).
confirm(dengue, [had_dengue]).
confirm(flu, [child, old]).
confirm(stomach_flu, []).
confirm(cholera, [low_bp, rapid_heart]).
confirm(rabies, [animal_wound]).
confirm(pneumonia, [old, smoker, low_oxygen, exposure]).
confirm(covid, [antigen_positive, exposure]).
confirm(hand_foot_mouth, [blister, child]).

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
    conclude(Disease).

% Determines the accuracy of the diagnosis
conclude(Disease) :-
    user_info(Information),
    confirm(Disease, DiseaseInfo),
    count_matching_items(Information, DiseaseInfo, H),
    length(DiseaseInfo, I),
    ((H >= 1, H < I); (H =:= 1, I =:= 1)), 
    capitalize_first_letter(Disease, DiseaseC),
    write(DiseaseC), write(' is likely your disease. Please consult a doctor.'),
    cleanup
    ;
    user_info(Information),
    confirm(Disease, DiseaseInfo),
    count_matching_items(Information, DiseaseInfo, H),
    length(DiseaseInfo, I),
    H > 1, H =:= I,
    capitalize_first_letter(Disease, DiseaseC),
    write(DiseaseC), write(' is indeed most likely your disease. Please consult a doctor.'),
    cleanup
    ;
    user_info(Information),
    confirm(Disease, DiseaseInfo),
    count_matching_items(Information, DiseaseInfo, H),
    length(DiseaseInfo, I),
    H =:= 0, I >= 1,
    capitalize_first_letter(Disease, DiseaseC),
    write(DiseaseC), write(' may not actually be your disease. Please consult a doctor.'),
    cleanup
    ;
    confirm(Disease, DiseaseInfo),
    length(DiseaseInfo, I),
    I =:= 0,
    write('Verifying whether or not you have '), write(Disease), write(' requires laboratory tests. Please consult a doctor and refer to a large medical facility.'),
    cleanup.

% Clears up the lists
cleanup :-
    user_info(A),
    user_symptoms(B),
    rejected_info(C),
    rejected_symptoms(D),
    retract(user_info(A)),
    retract(user_symptoms(B)),
    retract(rejected_info(C)),
    retract(rejected_symptoms(D)),
    assert(user_info([])),
    assert(user_symptoms([])),
    assert(rejected_info([])),
    assert(rejected_symptoms([])),
    nl, nl, write('Thanks for using our chat bot. To use it again, run "diagnose".').

% Runs the program upon consultation by calling diagnose/0
:- diagnose.