library(stringr)


string1 <- c('Data12', 'daFt', 'Youtube is fun', 'channel', 'WoW are you a student?')

#문자열 길이
str_length(string1)

str_length('abcdefg!!!')


#대소문자 변경

str_to_upper(string1)

str_to_lower(string1)

#단의 첫 글자만 대문자로 변경
str_to_title(string1)

#문장의 첫 글자만 대문자로 변경
str_to_sentence(string1)


#공백 제거
str_trim(' d ! ')


#공백 넣어줌
str_pad('pad this', width = 12, side = "both")


#문자열 자르기
str_trunc('hi how are you I am fine this motherfucking english is so basic bitch', width = 30)

#문자열 통합/분할
str_split('split this', pattern = ' ')

str_c('join', 'this', sep = '_')


str_c(c('join', 'this'), c('club', 'book'), sep = '.')


str_c(c('turn', 'me'), collapse = ' ')

#wtf with fastcampus examples ------

#결측지 NA를 문자열 NA로 변경할 수 있음
str_replace_na(c('make', NA, 'string!'))

#문자열 정렬
string2 <- c('sort', 'this', 'now')

str_order(string2)

str_sort(string2)


#삽입
first <- c('jihyun', 'inna', 'suhyun')
last <- c('jeon', 'ryu', 'kim')


str_glue('my name is {first} {last}')

#문자열 매치
data1 <- data.frame(author = c('kim', 'banana', 'cereal'),
                    message = c('apple was good but I like banana more. can I buy banana milk?',
                                'the apples were big, the banana was green. I will not buy the banana',
                                'I went to the grocery store'))

data1

#문자열 안에 특정 단어가 있는지 확인
str_detect(data1$message, pattern = 'banana')

#특정 단어가 있는 인덱스를 저장, 출력
idx <- str_which(data1$message, pattern = 'banana')
data1$message[idx]


str_count(data1$message, 'banana')

#단어의 시작 주소와 마지막 주소 출력(인덱시마다 처음 만날때까지)
str_locate(data1$message, 'banana')

#모두 출력
str_locate_all(data1$message, 'banana')


str_extract(data1$message, 'banana|big|the')


str_extract_all(data1$message, 'banana|big|the')


#문자열 자르기
str_subset(data1$message, pattern = 'banana')

str_sub(data1$message[1], start = 1, end = 5) <- str_to_upper(str_sub(data1$message[1], start = 1, end = 5))


#문자열 대체
str_replace(data1$message[1], pattern = 'banana', replacement = 'kiwi')

str_replace_all(data1$message[1], pattern = 'banana', replacement = 'kiwi')
