Quotes Language: german

# לוגיקה - שבוע 1

## הגדרות וחזרה

### מניה

להלן מספר הגדרות ומשפטים לחזרה.

#### קבוצה סופית

קבוצה \\( A \\) תקרא *סופית* אם קיים מספר טבעי \\( n \\) ופונקציה \\( h: \left\{ 0, 1, .., n-1 \right\} \rightarrow A \\) חח"ע ועל \\( A \\).

#### קבוצה בת-מניה

קבוצה \\( A \\) היא *בת-מניה* אם \\( A \\) סופית או \\( A \sim N \\).

#### סדרה סופית

סדרה סופית מעל \\( A \\) באורך \\( k \\) נראית כך:
\\[
    a_{0}, a_{1}, .., a_{k-1} \in A \\
    s = < a_{0}, a_{1}, .., a_{k-1}>
\\]

#### סדרה אינסופית

סדרה אינסופית מעל \\( A \\) היא פונקציה \\( s: \mathbb{N} \rightarrow A \\)

#### אוסף כל הסדרות האינסופיות

בנוסף להנ"ל, אם \\( A = \left\{ 0, 1 \right\} \\) , אז אוסף כל הסדרות האינסופיות מעל \\( A \\) היא קבוצה *בעוצמת הרצף* - כלומר בעוצמת \\( \mathbb{R} \\) - *ואיננה בת-מניה*.

#### מכפלה קרטזית

\\[ A \times B = \left\{ < a, b > \mid a\in A, b\in B \right\} \\]

##### משפט

<div markdown="1" align="center">
"אם \\( A, B \\) בנות מניה, גם \\( A \times B \\) בת-מניה." - גאורג קנטור
</div>

### קבוצה סדורה-חלקית (קס"ח)

\\( \leq \\) - סדר רפלקסיבי.  
\\( < \\) - סדר אי-רפלקסיבי.

#### סדר אי-רפלקסיבי

\\( (A,<) \\) נקרא סדר *אי-רפלקסיבי* אם \\( < \\) הוא יחס _טרנזיטיבי_ ואי-רפלקסיבי על \\( A \\) , משמע:

<div markdown="1" dir="LTR">
1. \\( \forall a, b \in A . (a \leq b) \wedge (b \leq c) \Rightarrow (a \leq c) \\)  
2. \\( \forall a \in A . \neg ( a < a ) \\)
</div>

#### קבוצה סדורה רפלקסיבית

\\( (A, \leq) \\) נקראת *קבוצה סדורה (חלקית) רפלקסיבית* אם:

1. \\( \leq \\) טרנזיטיבי על \\( A \\).
2. \\( \leq \\) רפלקסיבי על \\( A \\) : \\( \forall a \in A . (a \leq a) \\)
3. \\( \leq \\) אנטי-סימטרי : \\( \forall a \in A . (a \leq b) \wedge (b \leq a) \Rightarrow (a=b) \\)

#### קבוצה סדורה קווית

\\( (A, <) \\) נקראת *קבוצה סדורה קווית* (שלמה, לינארית) אם:

1. \\( < \\) סדר אי-רפלקסיבי על \\( A \\) .
2. מתקיים: \\( \forall a, b \in A . (a < b) \vee (b < a) \vee (a=b) \\)

#### איבר מקסימלי

אם \\( (A, \leq) \\) קס"ח, אז איבר \\( a \in A \\) נקרא *מקסימלי* אם אין \\( b \in A \\) המקיים \\( (b \neq a) \wedge (b \leq a) \\)

#### חסם מלעיל

\\( u \in A \\) חסם מלעיל עבור \\( X \subseteq A \\) אם \\( \forall x \in X . x \leq u \\)

#### שרשרת

\\( X \subseteq A \\) תקרא *שרשרת* אם \\( \forall a, b \in X . (a \leq b) \vee (b \leq a) \\)

##### דוגמא

יהי \\( A = \mathcal{P} ( \mathbb{R} ) \\) ונתבונתן *ביחס ההכלה*:

עבור \\( x, y \in A \\) נגדיר \\( x \leq y \\) אם מתקיים \\( x \subseteq y \\).
אזי \\( \mathbb{R} \\) הוא איבר *מקסימלי*, ומכיוון שהוא יחיד אז הוא גם *מקסימום*.

##### דוגמא לשרשרת אינסופית

1. כל הרישות של הטבעיים - שרשרת בת-מניה.
2. כל הרישות של הממשיים - שרשרת מעוצמת הרצף.

#### שרשרת מקסימלית

בדוגמא הנ"ל, נתבונן ב- \\( A = \mathcal{P} ( \mathbb{R} ) \setminus \mathbb {R} \\):  
לכל \\( \alpha \in \mathbb{R} \\) , הקבוצה \\( \mathbb{R} \setminus \left\{ \alpha \right\} \\) היא שרשרת מקסימלית.

<hr>

## משפט הסדר הטוב

<div markdown="1" align="center">
"כל קבוצה ניתנת לסידור טוב." - ארנסט צרמלו, 1908
</div>
![][Ernst Zermelo]

ארנסט פרידריך צרלמו (נולד ב-1871, נפטר ב-1953) היה מתמטיקאי ופילוסוף גרמני.
צרמלו נולד בברלין, בירת הקיסרות הגרמנית, ולאורך השנים למד מתמטיקה,
פיזיקה ופילוסופיה.
הדוקטורט שלו עסק בחשבון וריאציות,
ולאחר השלמת התואר היה לעוזרו של מקס פלאנק והחל לעסוק בהידרודינמיקה.
ב-1902 החל לעבוד על בעיות בתורת הקבוצות,
וב-1905 החל ביצירת בסיס אקסיומטי לתורת זו,
אך לא הצליח להוכיח את עקביות מערכת אקסיומות זו.
בשנת 1922 שכללו מתמטיקאים שונים את מערכת אקסיומות זו,
וכיום היא ידועה בשם אקסיומות צרמלו-פרנקל,
והיא המקובלת ביותר עד היום כבסיס לתורת הקבוצות האקסיומטית.
ב-1913 הוכיח צרמלו את משפט צרמלו, משפט חשוב בתורת המשחקים.
בשנת 1935 עזב צרמלו את תפקיד הכבוד שלו בפרייבורג בשל אי הסכמתו עם הלך
הרוחות הנאציים שם, אך חזר לאחר המלחמה וחי שם עד מותו.  
<br />
על מנת להוכיח את המשפט, ניעזר בלמה של צורן ובטענת עזר.

### סדר טוב

\\( (A, <) \\) נקרא *סדר טוב (Well Ordering)* אם:

1. \\( < \\) סדר אי-רפלקסיבי על A (קווי).
2. לכל \\( X \subseteq A \\) , אם \\( X \neq \varnothing \\) , אזי יש ב-\\( X \\) איבר מזערי (מינימום). כלומר, יש \\( x \in X \\) כך ש: \\( \forall y \in X . (x \neq y) \Rightarrow (x < y) \\)

**הערה:** מהגדרת הסדר הטוב נובע שכל קס"ח מסודר היטב או גם *קווי*.

<hr>

### הלמה של צורן

![][Max Zorn] מקס אוגוסט צורן (Max August Zorn, נולד ב-1906 בגרמניה, נפטר ב-1993 בארה"ב) היה מתמטיקאי גרמני-אמריקאי. צורן עסק באלגברה, בתורת הקבוצות ובאנליזה נומרית. התפרסם בעיקר בשל *הלמה של צורן*, למה שימושית במיוחד השקולה *לאקסיומת הבחירה*. כיהן כפרופ' באונ' אינדיאנה משנת 1946 ועד מותו. צורן היה נגן גיטרה נלהב. תמונה שלו מנגן בגיטרה תלויה בבניין הפקולטה למתמטיקה באוניברסיטת אינדיאנה.

#### הלמה

אם \\( A, \leq \\) קס"ח לא ריקה \\( ( A \neq \varnothing ) \\) המקיימת:

<div markdown="1" align="center">
לכל שרשרת \\( X \subseteq A \\) יש חסם מלעיל, כלומר קיים \\( a \in A \\) כך ש: \\( \forall a \in A . (x \leq a) \\)
</div> <br />
אז יש ב-\\( X \\) איבר מקסימלי, קרי איבר שאין גדול ממנו.

#### טענת עזר

נניח:

1. \\( I \\) קבוצה, ולכל \\( i \in I \\) , \\( ( A_{i} , \leq_{i} ) \\) קס"ח.
2. אם \\( i, j \in I \\) , אזי מתקיים \\( ( A_{i} , \leq_{j} ) \preceq ( A_{j} , \leq_{i} ) \\) או \\( ( A_{i} , \leq_{j} ) \succeq ( A_{j} , \leq_{i} ) \\)

אזי:

\\( ( A , \leq ) = ( \bigcup\limits_{i \in I} A_{i} , \bigcup\limits_{i \in I} \leq_{i} ) \\) קס"ח ומקיימת \\( \forall i \in I . ( A_{i} , \leq_{i} ) \preceq ( A , \leq ) \\)

##### הערות

* משמעות הביטוי \\( ( A , \leq_{A} ) \preceq ( B , \leq_{B} ) \\) :  
<div markdown="1" dir="LTR"> 1. \\( A \subseteq B \\)<br />2. \\( \forall a_{1} , a_{2} \in A . (a_{1} \leq_{A} a_{2}) \Leftrightarrow (a_{1} \leq_{B} a_{2}) \\) </div>

* \\( \preceq \\) - תת-סדר, משמע \\( \leq_{A} \subseteq \leq_{B} \\) . במילים פשוטות, הקס"ח \\( ( B, \leq_{B} ) \\) "עוטף מלמעלה" את \\( ( A, \leq_A ) \\) .

#### טענת עזר *

נניח:

כמקודם, ובנוסף נניח שאם \\( b \in B \setminus A \\) , אזי \\( \forall a \in A . a \leq_{B} b \\)

אזי:

\\( ( A , \leq ) = ( \bigcup\limits_{i \in I} A_{i} , \bigcup\limits_{i \in I} \leq_{i} ) \\) קס"ח **סדורה היטב**, ומקיימת \\( \preceq^{*} \\) כנ"ל.

##### הוכחת טענת העזר

נניח \\( \varnothing = X \subseteq \bigcup\limits_{i \in I} A_{i} \\) . צ"ל: מינימום ב-\\( X \\) .

נקח \\( x \in X \\) כלשהו, אזי יש \\( i \in I \\) כך ש \\( x \in A_{i} \\) .  
נגדיר \\( X' = X \cap A_{i} \\) . נשים לב ש-\\( X' \neq \varnothing \\) , כי בהכרח לפחות מתקיים \\( x \in X' \\) .

לכן, יש ב-\\( X' \\) מינימום לפי הסדר \\( \leq_{i} \\) . יהי \\( X_{0} \\) מינימום זה.  
ניתן להוכיח ש-\\( X_{0} \\) הוא גם מינימום של \\( X \\) בסדר \\( \bigcup\limits_{i \in I} \leq_{i} \\) .

### הוכחת משפט הסדר הטוב

נגדיר: \\[ P = \left\{ ( E , \leq_E ) \mid E \subseteq A ,\ \leq_E is\ well-ordered \right\} \\]

נישים לב ש-\\( P \neq \varnothing \\) .

נגדיר סדר חלקי \\( \preceq^{*} \\) על \\( P \\) :

<div markdown="1" align="center" dir="LTR">
\\( ( E , \leq_E ) \\) הרחבת-קצה של \\( ( F , \leq_F ) \Longleftrightarrow ( E , \leq_{E} ) \preceq^{*} ( F , \leq_F ) \\)
</div>
<br />
מטרה: להיעזר בלמה של צורן ע"מ למצוא איבר מקסימלי \\( ( E^M, \leq^M ) \\) ב-\\( P \\) , ולוודא ש-\\( E^{M} = A \\) .

ברור שאם \\( ( E^M , \leq^M ) \\) מקסימלי ב-\\( P \\) אז \\( E^M = A \\) , כי אחרת יש \\( e \in A \setminus E^{M} \\) ואז:

<div markdown="1" align="center" dir="LTR">
\\[ E^M , \leq^M ) \preceq^* ( E^M \cup \left\{ e \right\} , \leq^M \cup \left\{ ( x , e ) \mid x \in E^M \right\} \cup \left\{ (e, e) \right\} \\]
</div>
<br />
וזה יתן סתירה למקסימליות \\( ( E^M, \leq^M ) \\) .
<br />
<br />
נרצה להראות שלכל שרשרת \\( X \subseteq P \\) לא ריקה יש חסם מלעיל ב-\\( P \\) , אל זו בדיוק **טענת עזר\*** שראינו! ולכן:

<div markdown="1" align="center" dir="LTR">
\\[ X \ni ( A_i , \leq_i ) \subseteq^* ( \bigcup\limits_{i \in I} A_i , \bigcup\limits_{i \in I} \leq_i ) \in P \\]
</div>

<div markdown="1" dir="LTR">
\\( \square \\)
</div>

<hr>

## הלמה של קניג

![][Denes Konig]

דניס קניג (נולד ב-1884, נפטר ב-1944) היה מתמטיקאי יהודי-הונגרי,
אשר חקר ופרסם את הספר הראשון בנושא תורת הגרפים.
קניג נולד בבודפשט, בנו של המתמטיקאי גיולא קוניג.
ב-1907 זכה לתואר דוקטור והצטרף לפקולדט בטכניון בודפשט.
אחד מתלמידיו היה פול ארדוש (ממשפט ארדוש-ז'קרש),
אשר הצליח לפתור את אחת מבעיותיו בהיותו סטודנט שנה א'.
קוניג התאבד בשנת 1944 במהלך פרעות שואת יהודי הונגריה.

### הגדרות

#### עץ

סדר-חלקי \\( ( T , \leq_T ) \\) יקרא *עץ* אם לכל \\( t \in T \\) מתקיים:

\\[ \left\{ x \in T \mid x \leq_T t \right\} is\ well-ordered\ on \leq_T \\]

#### שורש

אם יש ל-\\( T \\) מינימום, אזי מינימום זה יקרא *שורש* העץ.

#### ענף

קבוצה סדורה קווית.

#### מרחק

אם לכל \\( t \in T \\) , \\( \left\{ x \in T \mid x < t \right\} \\) סופית ומכילה \\( k \\) איברים, אז נאמר ש-\\( k \\) היא המרחק של \\( t \\) מהשורש.

#### רמה

רמה \\( k \\) של עץ זה האוסף \\( \left\{ t \in T \mid length(t) = k \right\} \\) .

### טענה

אם \\( ( T , \leq_T ) \\) עץ ואם ל-\\( t \in T \\) יש \\( s \in T \\) כך ש \\( t \leq_T s \\) ,
אז יש \\( t < s_0 \leq s \\) כך ש \\( s_0 \\) עוקב מיידי של \\( t \\) .  
<br />
כלומר, אין \\( x \\) כך ש \\( t < x < s_0 \\) .

#### הוכחה

אם אין איבר בין \\( t \\) ל-\\( s \\) , אז \\( s \\) עוקב מיידי של \\( t \\) .  
אחרת \\( \left\{ x \in T \mid x < s \right\} \\) סדורה היטב (כי \\( T \\) עץ),
ו \\( \left\{ x \in T \mid t < x < s \right\} \\) תת-קבוצה לא ריקה שלה.  
לכן, יש בה מינימום \\( s_0 \\) שהוא עוקב מיידי של t.

##### הערה

נניח \\( ( T , \leq_T ) \\) סדר-חלקי המקיים שלכל \\( t \in T \\) , \\( \left\{x \in T \mid x \leq_T t \right\} \\) סופית וסדורה קווית, אז \\( ( T , \leq_T ) \\) הוא *עץ*.

### הלמה

נניח:

1. \\( ( T, \leq_T ) \\) עץ כך שלכל \\( k \in \mathbb{N} \\) יש רק מספר סופי של קודקודים \\( t \in T \\) עבורם \\( \left\{ x \in T \mid x \leq_T t \right\} \\) בעוצמה \\( k \\) .
2. \\( T \\) אינסופי.

אזי יש בעץ ענף אינסופי.

#### ניסוח נוסף

<div markdown="1" align="center">
"אם בעץ אינסופי, כל רמה \\( k \in \mathbb{N} \\) מכילה מספר סופי של קודקודים, אז יש בעץ ענף אינסופי."
</div>


[Ernst Zermelo]: http://upload.wikimedia.org/wikipedia/commons/7/7d/Ernst_Zermelo.jpeg width=150px
[Max Zorn]: http://upload.wikimedia.org/wikipedia/he/3/31/Zorn.jpeg width=150px
[Denes Konig]: http://upload.wikimedia.org/wikipedia/en/d/d0/Dénes_König.jpg width=150px