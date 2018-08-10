-module(udh_parser).

%% API exports
-export([pack/1, unpack/1]).

%%====================================================================
%% API functions
%%====================================================================
unpack(#{short_message := SM0} = Pdu) ->
	{UDH, SM} = split(SM0),
	if map_size(UDH) > 1 -> Pdu#{short_message => SM, udh => UDH};
		true -> Pdu#{short_message => SM}
	end;
unpack(#{<<"short_message">> := SM0} = Pdu) ->
	{UDH, SM} = split(SM0),
	UDH1 = maps:fold(fun(K, V, M) ->
					 	M#{atom_to_binary(K, utf8) => V}
					 end, #{}, UDH),
	if map_size(UDH) > 1 -> Pdu#{<<"short_message">> => SM, <<"udh">> => UDH1};
		true -> Pdu#{<<"short_message">> => SM}
	end;
unpack(Pdu) -> Pdu.

-define(HEADERS, [udh, <<"udh">>]).

pack(#{udh := UDH, short_message := SM} = Pdu) ->
	SM1 = unsplit(UDH, SM),
	maps:without(?HEADERS, Pdu#{short_message => SM1});
pack(#{<<"udh">> := UDH, <<"short_message">> := SM} = Pdu) ->
	SM1 = unsplit(UDH, SM),
	maps:without(?HEADERS, Pdu#{<<"short_message">> => SM1});
pack(Pdu) -> Pdu.

%%====================================================================
%% Internal functions
%%====================================================================

%------------------------------------------------------------------------------
% Ref : https://en.wikipedia.org/wiki/Concatenated_SMS#Sending_a_concatenated_SMS_using_a_User_Data_Header
%
% 8-bit CSMS
%  Field 1 (1 octet): Length of User Data Header, in this case 05.
%  Field 2 (1 octet): Information Element Identifier, equal to 00 (Concatenated
%                     short messages, 8-bit reference number)
%  Field 3 (1 octet): Length of the header, excluding the first two fields;
%                     equal to 03
%  Field 4 (1 octet): 00-FF, CSMS reference number, must be same for all the
%                     SMS parts in the CSMS
%  Field 5 (1 octet): 00-FF, total number of parts. The value shall remain
%                     constant for every short message which makes up the
%                     concatenated short message. If the value is zero then the
%                     receiving entity shall ignore the whole information
%                     element
%  Field 6 (1 octet): 00-FF, this part's number in the sequence. The value
%                     shall start at 1 and increment for every short message
%                     which makes up the concatenated short message. If the
%                     value is zero or greater than the value in Field 5 then
%                     the receiving entity shall ignore the whole information
%                     element. [ETSI Specification: GSM 03.40 Version 5.3.0:
%                     July 1996]
split(<<5, 0, 3, R:8, C:8, N:8, Rest/binary>>) ->
	{#{bit => 8, len => 5, info_elm_id => 0, hdr_len => 3, msg_ref_num => R,
	   total_segments => C, segment_seqnum => N}, Rest};
split([5, 0, 3, R, C, N | Rest]) ->
	{#{bit => 8, len => 5, info_elm_id => 0, hdr_len => 3, msg_ref_num => R,
	   total_segments => C, segment_seqnum => N}, Rest};

% 16-bit CSMS
%  It is possible to use a 16 bit CSMS reference number in order to reduce the
%  probability that two different concatenated messages are sent with identical
%  reference numbers to a receiver. In this case, the User Data Header shall be:
%
%  Field 1 (1 octet): Length of User Data Header (UDL), in this case 06.
%  Field 2 (1 octet): Information Element Identifier, equal to 08 (Concatenated
%                     short messages, 16-bit reference number)
%  Field 3 (1 octet): Length of the header, excluding the first two fields;
%                     equal to 04
%  Field 4 (2 octets): 0000-FFFF, CSMS reference number, must be same for all
%                     the SMS parts in the CSMS
%  Field 5 (1 octet): 00-FF, total number of parts. The value shall remain
%                     constant for every short message which makes up the
%                     concatenated short message. If the value is zero then the
%                     receiving entity shall ignore the whole information
%                     element
%  Field 6 (1 octet): 00-FF, this part's number in the sequence. The value shall
%                     start at 1 and increment for every short message which
%                     makes up the concatenated short message. If the value is
%                     zero or greater than the value in Field 5 then the
%                     receiving entity shall ignore the whole information
%                     element. [ETSI Specification: GSM 03.40 Version 5.3.0:
%                     July 1996]
split(<<6, 8, 4, R:16, C:8, N:8, Rest/binary>>) ->
	{#{bit => 16, len => 6, info_elm_id => 8, hdr_len => 4, msg_ref_num => R,
	   total_segments => C, segment_seqnum => N}, Rest};
split([6, 8, 4, RH, RL, C, N | Rest]) ->
	{#{bit => 16, len => 6, info_elm_id => 8, hdr_len => 4,
	   msg_ref_num => [RH, RL], total_segments => C, segment_seqnum => N},
	Rest};

% No UDH headers detected
split(Other) -> {#{}, Other}.

%------------------------------------------------------------------------------
% Ref : https://en.wikipedia.org/wiki/Concatenated_SMS#Sending_a_concatenated_SMS_using_a_User_Data_Header
%
% 8-bit CSMS
%  Field 1 (1 octet): Length of User Data Header, in this case 05.
%  Field 2 (1 octet): Information Element Identifier, equal to 00 (Concatenated
%                     short messages, 8-bit reference number)
%  Field 3 (1 octet): Length of the header, excluding the first two fields;
%                     equal to 03
%  Field 4 (1 octet): 00-FF, CSMS reference number, must be same for all the
%                     SMS parts in the CSMS
%  Field 5 (1 octet): 00-FF, total number of parts. The value shall remain
%                     constant for every short message which makes up the
%                     concatenated short message. If the value is zero then the
%                     receiving entity shall ignore the whole information
%                     element
%  Field 6 (1 octet): 00-FF, this part's number in the sequence. The value
%                     shall start at 1 and increment for every short message
%                     which makes up the concatenated short message. If the
%                     value is zero or greater than the value in Field 5 then
%                     the receiving entity shall ignore the whole information
%                     element. [ETSI Specification: GSM 03.40 Version 5.3.0:
%                     July 1996]
unsplit(#{<<"bit">> := 8, <<"len">> := 5, <<"info_elm_id">> := 0,
    	  <<"hdr_len">> := 3, <<"msg_ref_num">> := R,
		  <<"total_segments">> := C, <<"segment_seqnum">> := N}, SM) ->
		if is_list(SM) 	-> [5, 0, 3, R, C, N | SM];
			true 		-> <<5, 0, 3, R:8, C:8, N:8, SM/binary>>
		end;
unsplit(#{bit := 8, len := 5, info_elm_id := 0, hdr_len := 3, msg_ref_num := R,
		  total_segments := C, segment_seqnum := N}, SM) ->
		if is_list(SM) 	-> [5, 0, 3, R, C, N | SM];
			true 		-> <<5, 0, 3, R:8, C:8, N:8, SM/binary>>
		end;

% 16-bit CSMS
%  It is possible to use a 16 bit CSMS reference number in order to reduce the
%  probability that two different concatenated messages are sent with identical
%  reference numbers to a receiver. In this case, the User Data Header shall be:
%
%  Field 1 (1 octet): Length of User Data Header (UDL), in this case 06.
%  Field 2 (1 octet): Information Element Identifier, equal to 08 (Concatenated
%                     short messages, 16-bit reference number)
%  Field 3 (1 octet): Length of the header, excluding the first two fields;
%                     equal to 04
%  Field 4 (2 octets): 0000-FFFF, CSMS reference number, must be same for all
%                     the SMS parts in the CSMS
%  Field 5 (1 octet): 00-FF, total number of parts. The value shall remain
%                     constant for every short message which makes up the
%                     concatenated short message. If the value is zero then the
%                     receiving entity shall ignore the whole information
%                     element
%  Field 6 (1 octet): 00-FF, this part's number in the sequence. The value shall
%                     start at 1 and increment for every short message which
%                     makes up the concatenated short message. If the value is
%                     zero or greater than the value in Field 5 then the
%                     receiving entity shall ignore the whole information
%                     element. [ETSI Specification: GSM 03.40 Version 5.3.0:
%                     July 1996]
unsplit(#{<<"bit">> := 16, <<"len">> := 6, <<"info_elm_id">> := 8,
		  <<"hdr_len">> := 4, <<"msg_ref_num">> := R,
		  <<"total_segments">> := C, <<"segment_seqnum">> := N}, SM) ->
		if is_list(SM) 	-> lists:flatten([6, 8, 4, R, C, N | SM]);
			true 		-> <<6, 8, 4, R:16, C:8, N:8, SM/binary>>
		end;
unsplit(#{bit := 16, len := 6, info_elm_id := 8, hdr_len := 4,
		  msg_ref_num := R, total_segments := C, segment_seqnum := N}, SM) ->
		if is_list(SM) 	-> lists:flatten([6, 8, 4, R, C, N | SM]);
			true 		-> <<6, 8, 4, R:16, C:8, N:8, SM/binary>>
		end.

%------------------------------------------------------------------------------
-ifdef(TEST).
%%====================================================================
%% EUnit Tests
%%====================================================================

-include_lib("eunit/include/eunit.hrl").

-define(TESTS, [
	{"no udh atom", #{short_message => [1]}, #{short_message => [1]}},
	{"no udh bin", #{<<"short_message">> => <<1>>}, #{<<"short_message">> => <<1>>}},
	{"udh 8bit atom",
		#{udh => #{bit => 8, len => 5, info_elm_id => 0, hdr_len => 3,
					msg_ref_num => 10, total_segments => 2,
					segment_seqnum => 1}, short_message => [100]},
		#{short_message => [5,0,3,10,2,1,100]}},
	{"udh 8bit atom bin SM",
		#{udh => #{bit => 8, len => 5, info_elm_id => 0, hdr_len => 3,
					msg_ref_num => 10, total_segments => 2,
					segment_seqnum => 1}, short_message => <<100>>},
		#{short_message => <<5,0,3,10,2,1,100>>}},
	{"udh 8bit bin",
		#{<<"udh">> => #{<<"bit">> => 8, <<"len">> => 5,
							<<"info_elm_id">> => 0, <<"hdr_len">> => 3,
							<<"msg_ref_num">> => 10,
							<<"total_segments">> => 2,
							<<"segment_seqnum">> => 1},
			<<"short_message">> => <<100>>},
		#{<<"short_message">> => <<5,0,3,10,2,1,100>>}},
	{"udh 8bit bin string SM",
		#{<<"udh">> => #{<<"bit">> => 8, <<"len">> => 5,
							<<"info_elm_id">> => 0, <<"hdr_len">> => 3,
							<<"msg_ref_num">> => 10,
							<<"total_segments">> => 2,
							<<"segment_seqnum">> => 1},
			<<"short_message">> => [100]},
		#{<<"short_message">> => [5,0,3,10,2,1,100]}},
	{"udh 16bit atom",
		#{udh => #{bit => 16, len => 6, info_elm_id => 8, hdr_len => 4,
					msg_ref_num => [11,10], total_segments => 2,
					segment_seqnum => 1},
			short_message => [100]},
		#{short_message => [6,8,4,11,10,2,1,100]}},
	{"udh 16bit atom bin SM",
		#{udh => #{bit => 16, len => 6, info_elm_id => 8, hdr_len => 4,
					msg_ref_num => 16#0B0A, total_segments => 2,
					segment_seqnum => 1},
			short_message => <<100>>},
		#{short_message => <<6,8,4,11,10,2,1,100>>}},
	{"udh 16bit bin",
		#{<<"udh">> => #{<<"bit">> => 16, <<"len">> => 6,
							<<"info_elm_id">> => 8, <<"hdr_len">> => 4,
							<<"msg_ref_num">> => 16#0B0A,
							<<"total_segments">> => 2,
							<<"segment_seqnum">> => 1},
			<<"short_message">> => <<100>>},
		#{<<"short_message">> => <<6,8,4,11,10,2,1,100>>}},
	{"udh 16bit bin string SM",
		#{<<"udh">> => #{<<"bit">> => 16, <<"len">> => 6,
							<<"info_elm_id">> => 8, <<"hdr_len">> => 4,
							<<"msg_ref_num">> => [11,10],
							<<"total_segments">> => 2,
							<<"segment_seqnum">> => 1},
			<<"short_message">> => [100]},
		#{<<"short_message">> => [6,8,4,11,10,2,1,100]}}
	]
).

pack_test_() ->
    {inparallel,
     	[{Title, ?_assertEqual(Expected, pack(Pdu))}
      	|| {Title, Pdu, Expected} <- ?TESTS]}.

unpack_test_() ->
    {inparallel,
     	[{Title, ?_assertEqual(Expected, unpack(SM))}
      	|| {Title, Expected, SM} <- ?TESTS]}.

-endif.