%% DIRECTIVE
:-lambda_cut(0.5).

%% PROXIMITY RELATIONS
%% Location Distance Relationship
beverly_hills~downtown=0.3.
beverly_hills~santa_monica=0.45.
beverly_hills~hollywood=0.56.
beverly_hills~westwood=0.9.
downtown~hollywood=0.45.
downtown~santa_monica=0.23.
downtown~westwood=0.25.
hollywood~santa_monica=0.3.
hollywood~westwood=0.45.
santa_monica~westwood=0.9.

%% Category Relationship
comedy~drama=0.6.
comedy~adventure=0.3.
comedy~suspense=0.3.
drama~adventure=0.6.
drama~suspense=0.6.
adventure~suspense=0.9.

%% Films Table
%% film(Title, Director, Category)
film(four_feathers, korda, adventure).
film(modern_times, chaplin, comedy).
film(psycho, hitchcock, suspense).
film(rear_window, hitchcock, suspense).
film(robbery, yates, suspense).
film(star_wars, lucas, adventure).
film(surf_party, dexter, drama).

%% Theaters Table
%% theater(Name,Owner,Location).
theater(chinese,mann,hollywood).
theater(egyptian,va,westwood).
theater(music_hall,lae,beverly_hills).
theater(odeon,cineplex,santa_monica).
theater(rialto,independent,downtown).
theater(village,mann,westwood).

%% Engagements Table
%% engagement(Film,Theater)
engagement(modern_times,rialto).
engagement(start_wars,rialto).
engagement(star_wars,chinese).
engagement(rear_window,egyptian).
engagement(surf_party,village).
engagement(robbery,odeon).
engagement(modern_times,odeon).
engagement(four_feathers,music_hall).

%% MAIN RULE
%% search(input, input, output, output)
search(Category, Location, Film, Theater) :-
  film(Film, _, Category),
  engagement(Film, Theater),
  theater(Theater, _, Location).

%% GOAL EXAMPLES
% search(adventure, westwood, Film, Theater).
