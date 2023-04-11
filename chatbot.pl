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
symptom(chills).
symptom(hallucinations).
symptom(constipation).
symptom(nausea).
symptom(weakness).
symptom(runny_nose).
symptom(pus_in_stool).
symptom(joint_pain).
symptom(bodyache).
symptom(anxiety).
symptom(no_smell).
symptom(watery_diarrhea).
symptom(weight_loss).
symptom(irritable).
symptom(difficulty_breathing).
symptom(abdominal_pain).
symptom(vomiting).
symptom(hyperactivity).
symptom(abdominal_cramps).
symptom(weight_loss).
symptom(mucosal_dryness).
symptom(fever).
symptom(confusion).
symptom(increased_salivation).
symptom(sore_throat).
symptom(headache).
symptom(pink_eye).
symptom(rash).
symptom(night_sweats).
symptom(muscle_pain).
symptom(blood_in_stool).
symptom(feeling_thirsty).
symptom(fatigue).
symptom(elastosis).
symptom(no_taste).
symptom(blisters).
symptom(pain_behind_eyes).
symptom(diarrhea).
symptom(rapid_heart).
symptom(leg_cramps).
symptom(swollen_glands).
symptom(phlegm).
symptom(partial_paralysis).
symptom(insomnia).
symptom(feeling_unwell).
symptom(appetite_loss).
symptom(bloating).
symptom(chest_pain).
symptom(coughing_blood).
symptom(difficulty_swallowing).
symptom(common_cold).

% Defines diseases and their symptoms
disease(tuberculosis, [cough, fever, weakness, night_sweats, chest_pain, weight_loss, appetite_loss, coughing_blood, phlegm]).
disease(typhoid_fever, [fever, weakness, abdominal_pain, headache, diarrhea, constipation, cough, appetite_loss, rash]).
disease(dengue, [fever, headache, pain_behind_eyes, muscle_pain, joint_pain, nausea, vomiting, swollen_glands, rash]).
disease(flu, [fever, chills, cough, sore_throat, common_cold, muscle_pain, bodyache, headache, fatigue, vomiting, diarrhea]).
disease(stomach_flu, [fever, appetite_loss, bloating, nausea, vomiting, abdominal_pain, abdominal_cramps, diarrhea, blood_in_stool, pus_in_stool, feeling_unwell]).
disease(cholera, [diarrhea, watery_diarrhea, vomiting, feeling_thirsty, leg_cramps, irritable, elastosis, mucosal_dryness]).
disease(rabies, [insomnia, anxiety, confusion, partial_paralysis, hyperactivity, irritable, hallucinations, increased_salivation, difficulty_swallowing]).
disease(pneumonia, [fever, cough, chest_pain, phlegm, coughing_blood, chills, difficulty_breathing, appetite_loss, fatigue, nausea, vomiting, confusion]).
disease(covid, [fever, common_cold, sore_throat, cough, no_taste, no_smell, difficulty_breathing, chills, headache, chest_pain, pink_eye, nausea, vomiting, diarrhea, rash]).
disease(hand_foot_mouth, [fever, sore_throat, feeling_unwell, rash, irritable, appetite_loss]).

% Defines diagnostic tests or other information associated with the illness
question(young_adult, 'Are you a young adult/adolescent (less than 24 years old)?').
question(young_old, 'Are you an infant, young child, or a senior older than 65?').
question(child, 'Are you a child younger than 5 years old?').
question(smoker, 'Do you smoke?').
question(had_dengue, 'Have you had dengue before?').
question(had_respiratory_infection, 'Have you had a respiratory infection recently?').
question(weak_immunity, 'Do you have a weakened immune system?').
question(low_oxygen, 'Measure your blood oxygen. Is it low?').
question(low_bp, 'Measure your blood pressure. Is it low?').
question(rapid_heart, 'Measure your heart rate. Is it high?').
question(antigen_positive, 'Perform a rapid antigen test. Is the result positive?').
question(animal_wound, 'Have you been bitten or scratched by an animal?').
question(animal_exposure, 'Are you frequently exposed to stray dogs?').
question(tb_exposure, 'Have you been exposed to people with tuberculosis recently?').
question(flu_exposure, 'Have you been exposed to people exhibiting flu symptoms recently?').
question(covid_exposure, 'Have you been exposed to people exhibiting COVID symptoms recently?').
question(exposed, 'Are you frequently in crowded areas or congregate settings?').
question(poor_sanitation, 'Do you live in an area with poor sanitation and hygiene?').

% Defines which information can be used to confirm which disease
confirm(tuberculosis, [weak_immunity, smoker, tb_exposure, exposed]).
confirm(typhoid_fever, [poor_sanitation]).
confirm(dengue, [poor_sanitation, had_dengue]).
confirm(flu, [flu_exposure]).
confirm(stomach_flu, [poor_sanitation]).
confirm(cholera, [low_bp, rapid_heart]).
confirm(rabies, [animal_exposure, animal_wound]).
confirm(pneumonia, [young_old, smoker, low_oxygen, had_respiratory_infection]).
confirm(covid, [antigen_positive, covid_exposure]).
confirm(hand_foot_mouth, [child, poor_sanitation]).

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
        user_symptoms(Symptoms),
        length(Symptoms, S),
        S > 1,
        write('Based on your symptoms, you may have '), write(Y), write('.'), nl,
        write('You are experiencing '), write(N), write(' out of '), write(M), write(' of its possible symptoms.'), nl, nl,
        write('Let\'s try to confirm your disease...'), nl,
        verify(Y)
    )
    ;
    % If too few symptoms match
    user_symptoms(Symptoms),
    length(Symptoms, S),
    S =:= 1,
    write('You have too few symptoms to be diagnosed a disease, or your disease may not be in my database.'), nl,
    write('Please consult a doctor or refer to a large medical facility.'), nl,
    cleanup
    ;
    % If user has no symptoms
    user_symptoms(Symptoms),
    length(Symptoms, S),
    S =:= 0,
    write('I cannot determine a diagnosis as you are not exhibiting any of the symptoms in my database.'), nl,
    write('You may not have a disease, but if you feel unwell, please consult a doctor or refer to a large medical facility.'), nl,
    cleanup
    ;
    % Error
    write('Sorry, I could not determine a proper diagnosis.'), nl,
    write('Please consult a doctor or refer to a large medical facility.'), nl,
    cleanup.

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
    write(DiseaseC), write(' is likely your disease.'), nl,
    write('Please refer to a large medical facility to verify this diagnosis.'), nl,
    cleanup
    ;
    user_info(Information),
    confirm(Disease, DiseaseInfo),
    count_matching_items(Information, DiseaseInfo, H),
    length(DiseaseInfo, I),
    H > 1, H =:= I,
    capitalize_first_letter(Disease, DiseaseC),
    write(DiseaseC), write(' is indeed most likely your disease.'), nl,
    write('Please seek treatment or refer to a large medical facility to further verify this diagnosis.'), nl,
    cleanup
    ;
    user_info(Information),
    confirm(Disease, DiseaseInfo),
    count_matching_items(Information, DiseaseInfo, H),
    length(DiseaseInfo, I),
    H =:= 0, I >= 1,
    capitalize_first_letter(Disease, DiseaseC),
    write(DiseaseC), write(' may not actually be your disease.'), nl,
    write('To get an accurate diagnosis, please refer to a large medical facility.'), nl,
    cleanup
    ;
    confirm(Disease, DiseaseInfo),
    length(DiseaseInfo, I),
    I =:= 0,
    write('Verifying whether or not you have '), write(Disease), write(' requires laboratory tests.'), nl,
    write('Please consult a doctor and refer to a large medical facility.'), nl,
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
    nl, nl, write('Thanks for using Medical Expert System! To use it again, type \'yes\': '), nl,
    read(Response),
    (Response == 'yes' -> diagnose; goodbye).

% Upon quit
goodbye :-
    write('Goodbye!').

% Runs the program upon consultation by calling diagnose/0 and prints welcome message
main :-
    write('Welcome to the Medical Expert System!'), nl,
    write('Let\'s get started.'), nl, nl,
    diagnose.

:- main.