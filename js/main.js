/**
 * Created by HUQ on 10/22/15.
 */
/**
 * Created by HUQ on 9/22/15.
 */
"use strict";

var ngApp = angular.module('rhymes', []);
console.log("ngApp is running");
var countDown;


ngApp.controller('FindARhyme', function($scope, $window) {
  console.log("We are in FindARhyme");
  var timeCount = 4;
  var timerForSlideChange;
  var audio = new Audio('assets/EpicNinjaTurtleRapInstrumental.wav');
  $scope.showLogo = "assets/blackoriginalorderfw.png";
  $scope.isNoneOrBlock = "none";

  var players = {
    jameela:'#A95E5F' , neil: '#5B8498', art: '#8276BF', kyle: '#E5A37F'
  };

  var colorCode = {
    rapper1: players['kyle'],
    rapper2: players['jameela'],
    rapper3: players['neil'],
    rapper4: players['art'],
    none: 'black'
  };

  //select words button saves array and shows START button
  //during the countdown, escape is possible via keypress


  //var oneSyllWords = ["act","add","aft","alp","am","amp","an","and","ant","apt","as","ash","ask","asp","at","ax","back","bad","badge","bag","ban","band","bang","bank","bask","bass","bash","bat","batch","bath","black","bland","blank","blast","brag","bran","branch","brand","brass","cab","calf","calves","camp","can","cap","cash","cask","cast","cat","catch","chaff","chance","chant","chap","chasm","chat","clam","clamp","clan","clap","clash","clasp","class","crab","crack","craft","cram","cramp","crash","dab","dad","daft","dam","damp","dance","dank","dash","drab","draft","drag","drank","fact","fad","fan","fang","fast","fat","fax","flack","flag","flank","flap","flash","flask","flat","frank","gag","gang","gap","gas","gash","gasp","glad","glance","gland","glass","gnat","grab","graft","gram","grand","grant","graph","grasp","grass","hack","had","hag","half","halves","ham","hand","hang","has","hat","hatch","have","jab","jack","jam","jazz","knack","lack","lad","lag","lamb","lamp","lance","land","lap","lapse","lash","lass","last","latch","laugh","lax","mad","man","map","mash","mask","mass","mast","mat","match","math","nag","nap","pack","pact","pad","pal","pan","pant","pants","pass","past","pat","patch","path","plan","plank","plant","plaque","prank","quack","rack","raft","rag","ram","ramp","ranch","rank","rap","rash","rat","sack","sad","sag","sand","sang","sank","sap","sash","sat","scab","scalp","scan","scat","scrap","scratch","shack","shaft","shag","slab","slack","slam","slang","slant","slap","slash","smack","smash","snack","snap","snatch","span","spank","spat","splash","stab","stack","staff","stag","stamp","stand","strap","swam","tab","tack","tact","tag","tan","tang","tank","tap","task","tax","thrash","than","that","thatch","track","tract","tramp","trap","trash","valve","van","vast","vat","wag","wax","whack","wrap","wrath","zap","bed","beg","bell","belch","belt","bench","bend","bent","best","bet","blend","bless","bred","cell","cent","check","chef","chess","chest","crept","cress","crest","debt","deck","den","dense","dent","depth","desk","dredge","drench","dress","dwell","ebb","edge","egg","elf","elk","elm","else","end","fed","fell","felt","fence","fend","fetch","fled","flesh","flex","fresh","gel","gem","get","glen","guess","guest","held","helm","help","hem","hen","jest","jet","kept","keg","led","ledge","leg","left","lend","length","lens","lent","less","let","men","melt","mend","mesh","mess","met","neck","nest","net","next","peck","peg","pelt","pen","pest","pet","pledge","press","quench","quest","red","rent","rest","scent","sect","self","sell","selves","send","sense","sent","set","sex","shed","shelf","shell","shred","sketch","sled","slept","smell","speck","spell","spend","spent","stem","stench","step","stress","stretch","strength","swell","swept","tell","tempt","ten","tend","tense","","tent","test","text","theft","them","then","trek","trench","trend","vent","vet","vex","web","wedge","weld","well","welt","went","wept","west","wet","when","wreck","wren","wrench","zest","bib","bid","big","bill","bin","bit","blink","bliss","brick","bridge","brim","bring","brink","brisk","build","chick","chill","chimp","chin","chink","chip","cliff","cling","clip","crib","cringe","crisp","did","dig","dim","din","dip","disc","dish","disk","ditch","drift","drill","drink","drip","fib","fig","film","fin","finch","fish","fist","fit","fix","fizz","flick","fling","flint","flip","frill","fringe","frisk","gift","gig","gill","gin","gist","give","glimpse","grid","grill","grim","grin","grip","grit","guilt","hid","hill","hilt","him","hinge","hint","hip","his","hiss","hit","hitch","if","ill","imp","in","inch","ink","inn","is","it","itch","its","jig","jinx","kick","kid","kill","kiln","kilt","kin","king","kiss","kit","knit","lick","lid","lift","limb","limp","link","lip","lisp","list","lit","live","milk","mill","mince","mint","miss","mitt","nib","nil","nip","pick","pig","pill","pin","pinch","ping","pink","pip","pit","pitch","prick","","prim","prince","print","prism","quick","quill","quilt","quit","quiz","rib","rich","rid","ridge","rift","rig","rim","ring","rinse","rip","risk","script","shift","shin","ship","shrill","shrimp","shrin","k","sick","sift","silk","sill","silt","sin","since","sing","singe","sink","sip","sit","six","skid","skill","skim","skin","skip","slick","slid","slim","slip","slit","sniff","snip","spill","spin","spit","splint","split","sprig","spring","sprint","squid","squint","stick","stiff","still","stilts","sting","stink","stitch","strict","string","strip","swift","swim","swing","swish","switch","thick","thin","thing","think","this","thrill","tick","till","tilt","tin","tint","tip","trick","trim","trip","twig","twin","twist","twitch","which","whim","whip","whisk","whiz","wick","width","wig","will","wilt","win","winch","wind","wing","wink","wish","wit","witch","with","wring","wrist","zinc","zip","blob","block","blonde","blot","bob","bog","bomb","bond","boss","box","bronze","broth","chop","clock","clod","clog","cloth","cock","cod","cog","come","con","cop","cost","cot","cough","crop","cross","dock","dodge","dog","doll","done","dot","dove","drop","flock","flog","flop","fog","fond","font","fox","frock","frog","from","front","frost","froth","gloss","glove","god","golf","gone","gong","gosh","got","hog","hop","hot","job","jog","jot","knob","knock","knot","lob","lock","lodge","loft","log","long","lop","loss","lost","lot","love","mob","mock","mom","monk","month","mop","mosque","moss","moth","nod","none","not","notch","odd","off","on","once","one","opt","ox","plod","plot","pod","pond","pop","pot","prod","prompt","prong","prop","rob","rock","rod","rot","rough","scoff","shock","shop","shot","shove","slog","slop","slot","sloth","smog","snob","sob","sock","soft","solve","some","son","song","","sop","sponge","spot","stock","stop","strong","throb","throng","ton","tongs","tongue","top","toss","tot","tough","trot","trough","won","wrong","bluff","blunt","blush","brush","buck","bud","budge","bug","bulb","bulge","bump","bun","bunch","bus","bust","but","butt","buzz","chuck","chug","chunk","club","cluck","clump","clung","clutch","crumb","crunch","crush","crust","crutch","cub","cud","cuff","cup","cut","drug","drum","drunk","duck","dug","dull","dumb","dump","dun","dung","dusk","dust","fluff","flung","flush","fudge","fun","fund","fuss","glum","fuzz","grub","grudge","grump","gulf","gull","gulp","gum","gun","gush","gut","hub","huff","hug","hull","hum","hump","hunch","hung","hunk","hunt","hush","hut","hutch","judge","jug","jump","junk","just","jut","luck","lull","lump","lunch","lung","lunge","lush","lust","much","muck","mud","mug","mumps","munch","must","mutt","nudge","numb","nun","nut","pluck","plug","plum","plump","plunge","plus","puck","puff","pulp","pulse","pump","pun","punch","pus","putt","rub","rug","rum","rump","run","rung","rush","rust","rut","scruff","scrub","","scum","shrub","shrug","shun","shut","skull","skunk","slug","slum","slump","smudge","smug","snug","struck","stub","stuck","stuff","stump","stun","stung","stunk","stunt","strut","such","suck","sulk","sum","sun","sung","sunk","swum","swung","thrush","thrust","thud","thug","thumb","thus","trudge","truck","trunk ","trust","tub","tuck","tuft","tug","tusk","up","us","wrung","bay","clay","day","gay","gray","hay","lay","may","pay","play","pray","ray","say","slay","spray","stay","stray","sway","tray","way","aid","aim","air","bail","bait","braid","brain","chain","chair","claim","drain","fail","faint","fair","faith","flair","frail","gain","grain","hail","hair","jail","laid","lain","lair","maid","mail","maim","main","nail","paid","pail","pain","paint","pair","plain","praise","quail","quaint","raid","rail","rain","raise","sail","saint","snail","sprain","stain","stair","stairs","strain","strait","tail","trail","train","vain","wail","waist","wait","ace","age","ale","ape","ate","bale","bare","base","blade","blame","blare","blaze","brace","brake","brave","cage","cake","came","cane","cape","care","case","cave","chase","crane","crate","crave","craze","dale","dare","date","daze","drake","face","fade","fake","fame","fare","fate","flake","flame","flare","frame","gale","","game","gape","gate","gave","gaze","glare","glaze","grace","grade","grape","grate","grave","graze","hare","hate","haze","jade","knave","lace","lake","lame","lane","late","made","make","male","mane","mare","mate","maze","name","pace","page","pale","pane","pave","phase","phrase","place","plane","plate","quake","race","rage","rake","rare","rate","rave","raze","safe","sage","sake","sale","same","sane","save","scale","scare","scrape","shade","shake","shame","shape","share","shave","skate","slate","slave","snake","snare","space","spade ","spare","square","stage","stake","stale","stare","state","take","tale","tame","tape","trace","trade","vale","vase","wade","wake","wane","wave","whale","wage","break","great","pear","steak","swear","tear","wear","eight","freight","neigh","sleigh","weigh","weight","prey","they","whey","ache","bass","bathe","change","haste","paste","plague","range","strange","taste","vague","waste","heir","reign","reins","their","veil","vein","be","he","me","she","the","we","bee","beech","beef","beep","beer","beet","bleed","breed","breeze","cheek","cheep","cheer","cheese","creek","creep","deed","deep","deer","eel","fee","feed","feel","feet","flee","fleet","free","freeze","geese","glee","greed","green","greet","heel","jeep","jeer","keel","keen","keep","knee","kneel","leek","meek","meet","need","peek","peel","peep","peer","preen","queen","queer","reed","reef","reek","reel","screen","see","seed","seek","seem","seen","seep","seethe","sheen","sheer","sheet","sleek","sleep","sleet","sleeve","sneer","sneeze","speech","speed","squeeze","steel","steep","steer","street","sweep","sweet","teem","teen","teeth","teethe","three","tree","veer","weed","week","weep","wheel","wheeze","beach","bead","beak","beam","bean","beard","beast","beat","bleach","bleak","breathe","cease","cheap","cheat","clean","clear","creak","cream","crease","deal","dear","dream","each","ear","ease","east","eat","fear","feast","feat","flea","gear","gleam","","grease","heal","heap","hear","heat","heave","jeans","knead","lead","leaf","league","leak","leap","lease","least","leave","meal","mean","meat","near","neat","pea","peace","peak","plea","plead","please","preach","reach","read","real","reap","rear","scream","sea","seal","seam","seat","shear","shears","sheath","smear","sneak","speak","spear","squeak","squeal","steal","steam","streak","stream","tea","teach","team","tear","tease","treat","veal","weak","wean","weave","wheat","wreath","year","yeast","zeal","brief","chief","field","fiend","fierce","grief","grieve","niece","piece","pier","pierce","priest","shield","shriek","siege","thief","tier","yield","key","quiche","ski ","suite","seize","weird","eve","gene","here","scene","scheme","sphere","theme","these","bind","blind","child","climb","cried","dried","find","fried","grind","hind","iced","kind","mild","mind","pint","rind","sign","spied","tried","wild","die","cries","lie","lies","pie","pies","spies","tie","tries","vie","bike","bite","bribe","bride","brine","chime","chive","crime","dice","dike","dime","dine","dire","dive","drive","file","fine","fire","five","glide","grime","guide","hide","hike","hire","hive","ice","kite","knife","lice","life","like","lime","line","live","lives","mice","mile","mime","mine","nice","nine","pike","pile","pine","pipe","price","pride","prize","quite","rice","ride","ripe","rise","shine","shrine","side","site","size","slice","slide","slime","smile","spice","spike","spine","spire","spite","stride","strife","strike","stripe","strive","thrive","tide","tile","time","tire","tribe","twine","vice","vile","vine","vise","while","whine","white","wide","wife","wine","wipe","wire","wise","write","","by","cry","dry","fly","fry","guy","my","pry","rhyme","shy","sky","sly","spy","sty","try","why","bright","fight","flight","fright","high","knight","light","might","night","plight","right","sigh","sight","slight","tight","tights","thigh","bold","bolt","both","clothes","cold","colt","comb","fold","folk","ghost","go","gold","gross","hold","host","jolt","mold","molt","most","no","old","poll","post","rogue","roll","scold","scroll","sold","stroll","told","toll","troll","vogue","volt","boar","board","boast","boat","cloak","coach","coal","coarse","coast","coat","coax","float","foal","foam","goal","goat","groan","hoard","hoarse","hoax","load","loaf","loan","loathe","loaves","moan","moat","oak","oar","oath","oats","poach","road","roam","roar","roast","soak","soap","soar","throat","toad","toast","doe","foe","hoe","poem","toe","woe","bone","bore","broke","clone","close","choke","chore","chrome","close","code","coke","cone","cope","core","cove","dole","dome","dose","doze","drone","drove","fore","globe","gnome","gore","grope","grove","hole","home","hope","hose","joke","lone","mole","","mope","more","nose","note","ore","phone","poke","pole","pope","pore","pose","probe","quote","robe","rode","role","rope","rose","scone","scope","score","shone","shore","slope","smoke","snore","sole","sore","spoke","stone","store","stove","strode","stroke","strove","swore","those","throne","tone","tore","tote","vote","whole","woke","wore","wove","wrote","zone","blow","bow","bowl","crow","flow","flown","glow","grow","growth","know","low","mow","own","row","show","slow","snow","sow","throw","tow","court","course","four","mourn","pour","soul","source","dough","though","cue","fuel","hue","cube","cure","cute","fume","fuse","huge","mule","pure","sure","use","cubed","cued","cured","fumed","fused","used","few","pew","arc","arch","ark","arm","art","bar","barge","bark","barn","car","card","carp","cart","carve","char","charge","charm","chart","dark","darn","dart","far","farce","farm","guard","hard","harm","harsh","jar","harp","lard","large","lark","mar","march","mark","marsh","parch","park","part","scar","scarf","shark","sharp","smart","snarl","spark","sparse","star","starch","start","starve","tar","tarp","tart","born","borne","chord","cord","corn","cork","corpse","force","forge","ford","fork","form","fort","gorge","horn","horse","lord","north","porch","pork","port","scorn","scorch","shorn","short","snort","sort","sport","stork","storm","sword","sworn","thorn","torch","torn","dwarf","quart","quartz","swarm","war","ward","warm","warn","warp","wart","wharf","clerk","fern","germ","her","herd","jerk","merge","nerve","perch","perm","serf","serve","sperm","swerve","term","verb","verge","verse","bird","birth","chirp","dirt","fir","firm","first","flirt","girl","shirk","shirt","sir","skirt","squirt","stir","swirl","third","thirst","twirl","whirl","blur","burn","burp","burst","church","churn","curb","curl","curse","curve","fur","hurl","hurt","lurch","lurk","nurse","purge","purse","spur","spurt","surf","turf","turn","urge","urn","earl","earn","heard","hearse","learn","pearl","search","word","work","world","worm","worse","worst","worth","there","where","scarce","bow","brow","brown","browse","clown","cow","crowd","crown","down","drown","fowl","frown","gown","growl","how","howl","now","plow","prowl","scowl","sow","town","vow","blouse","bounce","bound","bout","cloud","clout","couch","count","crouch","doubt","dour","foul","found","grouch","ground","hound","hour","house","joust","loud","lounge","louse","mound","mount","mouse","mouth","noun","pound","ounce","out","pouch","pounce","pout","proud","round","rouse","route","scour","scout","scrounge","shout","slouch","snout","sound","sour","south","spouse","spout","sprout","stout","trout","wound","bough","drought","boy","coy","joy","toy","boil","broil","choice","coil","coin","foil","join","joint","moist","noise","oil","oink","point","roil","soil","spoil","toil","voice","bloom","boom","boost","boot","brood","broom","choose","cool","doom","drool","droop","food","fool","goose","groom","groove","hoop","hoot","hooves","loom","loop","loose","loot","mood","moon","moor","moose","noon","noose","ooze","pool","proof","room","roost","school","scoop","shoot","smooth","snoop","soon","soothe","spoon","stool","stoop","swoop","too","tool","tooth","troop","zoo","zoom","blew","brew","chew","crew","dew","drew","flew","knew","new","news","newt","screw","shrew","stew","shrewd","blue","clue","due","glue","sue","true","brute","crude","duke","dune","flute","June","lure","nude","prune","rude","rule","tube","tune","truce","lose","move","prove","tomb","two","who","whom","whose","womb","fruit","juice","bruise","cruise","group","soup","tour","wound","you","youth","flu","truth","through","book","brook","cook","crook ","good","hood","hoof","hook","look","roof","rook","root","shook","soot","took","wood","wool","bull","bush","full","pull","push","put","could","should","would","awl","bawl","brawl","claw","crawl","dawn","draw","drawn","fawn","flaw","gnaw","hawk","jaw","law","lawn","paw","pawn","prawn","raw","saw","scrawl","shawl","spawn","sprawl","squawk","straw","thaw","yawn","cause","clause","fault","fraud","gauze","haul","haunt","launch","maul","pause","sauce","taunt","taut","vault","caught","taught","bought","brought","fought","sought","thought","bread","breadth","breath","breast","dead","deaf","death","dread","head","health","lead","leapt","meant","read","realm","spread","sweat","thread","threat","tread","wealth","said","crypt","gym","hymn","myth","all","bald","balk","ball","call","calm","chalk","fall","false","hall","halt","mall","malt","palm","quad","salt","scald","small","squad","squash","squat","stall","swamp","swan","swap","swat","stalk","talk","tall","walk","wall","waltz","wand","want","was","wash","wasp","watch","watt","what","yacht","touch","young"]

  var nounList = ["people", "way", "world", "map", "family", "health", "computer", "year", "music", "reading", "data", "understanding", "law", "literature", "software", "knowledge", "ability", "love", "television", "library", "fact", "idea", "investment", "society", "story", "media", "oven", "definition", "quality", "language", "player", "video", "security", "exam", "organization", "physics", "policy", "thought", "boyfriend", "strategy", "army", "freedom", "environment", "instance", "truth", "university", "article", "difference", "news", "fishing", "income", "user", "failure", "medicine", "teacher", "night", "disease", "energy", "road", "soup", "location", "addition", "education", "moment", "politics", "decision", "property", "student", "competition", "entertainment", "population", "unit", "cigarette", "introduction", "performance", "flight", "magazine", "relationship", "cell", "finding", "member", "phone", "appearance", "concept", "death", "housing", "insurance", "woman", "blood", "expression", "opinion", "reality", "situation", "statement", "application", "county", "estate", "grandmother", "perspective", "recipe", "topic", "depression", "passion", "resource", "college", "criticism", "description", "patience", "solution", "aspect", "director", "psychology", "response", "storage", "alcohol", "complaint", "emphasis", "loss", "possession", "steak", "agreement", "currency", "engineering", "interaction", "preference", "republic", "virus", "classroom", "device", "drama", "engine", "guidance", "owner", "protection", "tension", "anxiety", "awareness", "bread", "climate", "confusion", "elevator", "employee", "guest", "leadership", "manager", "recording", "transportation", "cousin", "editor", "excitement", "feedback", "homework", "mom", "permission", "promotion", "refrigerator", "revenue", "singer", "basket", "cabinet", "church", "coffee", "drawing", "hearing", "judgment", "measurement", "mud", "poetry", "possibility", "queen", "relation", "satisfaction", "signature", "song", "town", "volume", "accident", "appointment", "assumption", "chapter", "conversation", "enthusiasm", "explanation", "gate", "hall", "hospital", "instruction", "manufacturer", "perception", "poem", "proposal", "replacement", "river", "speech", "village", "winner", "writer", "breath", "chest", "conclusion", "cookie", "dad", "drawer", "examination", "grocery", "impression", "independence", "inspection", "king", "menu", "piano", "profession", "quantity", "requirement", "sister", "tongue", "wedding", "ambition", "apple", "assistant", "bedroom", "birthday", "championship", "client", "departure", "dirt", "fortune", "funeral", "girlfriend", "indication", "lady", "negotiation", "passenger", "platform", "pollution", "reputation", "sir", "stranger", "sympathy", "throat", "uncle", "time", "film", "money", "while", "study", "life", "air", "place", "part", "fish", "process", "hand", "job", "end", "type", "economy", "body", "guide", "state", "course", "price", "card", "mind", "line", "group", "word", "force", "light", "name", "top", "level", "practice", "sense", "piece", "boss", "fun", "page", "test", "sound", "matter", "soil", "oil", "access", "range", "reason", "site", "exercise", "case", "coast", "age", "boat", "result", "building", "cash", "nothing", "plan", "tax", "subject", "rule", "weather", "figure", "model", "beginning", "program", "design", "head", "purpose", "rock", "act", "car", "object", "sun", "profit", "speed", "war", "craft", "inside", "standard", "exchange", "fire", "pressure", "advantage", "box", "issue", "cycle", "item", "paint", "room", "structure", "account", "discipline", "share", "bit", "bottom", "gift", "machine", "tool", "address", "career", "morning", "sign", "task", "contact", "egg", "ice", "north", "attempt", "effect", "post", "voice", "challenge", "self", "brush", "debate", "front", "lack", "plant", "spot", "taste", "track", "brain", "click", "foot", "influence", "rain", "base", "distance", "pair", "staff", "target", "animal", "budget", "file", "lesson", "officer", "reference", "sky", "stick", "trouble", "bridge", "character", "edge", "fan", "lock", "novel", "pack", "plenty", "skin", "weight", "background", "dish", "fruit", "joint", "muscle", "strength", "trip", "appeal", "gear", "kitchen", "log", "net", "principle", "sale", "signal", "street", "wave", "bench", "copy", "minimum", "progress", "sea", "status", "ticket", "angle", "breakfast", "daughter", "doctor", "dream", "essay", "fee", "hour", "limit", "milk", "peace", "seat", "storm", "team", "afternoon", "beach", "catch", "consideration", "crew", "gold", "kid", "match", "pain", "score", "sex", "shower", "tone", "agent", "block", "calendar", "coat", "corner", "cup", "door", "finger", "guarantee", "hook", "layer", "lie", "meeting", "parking", "profile", "rice", "schedule", "telephone", "winter", "bag", "bed", "bother", "code", "designer", "dress", "emergency", "extension", "fight", "grade", "horror", "host", "loan", "mountain", "noise", "package", "pause", "proof", "relief", "sentence", "smoke", "string", "towel", "west", "wine", "aside", "bet", "border", "breast", "buddy", "chip", "cross", "draft", "expert", "god", "habit", "judge", "landscape", "mail", "native", "parent", "pin", "pound", "salary", "shelter", "silver", "tank", "assist", "bar", "bike", "boy", "chair", "clue", "comment", "devil", "fear", "glove", "lunch", "mortgage", "pace", "peak", "reward", "sandwich", "spite", "surprise", "transition", "welcome", "alarm", "bicycle", "blind", "cable", "clerk", "concert", "flower", "harm", "lawyer", "load", "neck", "plate", "ruin", "skirt", "snow", "stroke", "trash", "zone", "award", "bitter", "bug", "candy", "cat", "channel", "comfort", "crack", "entrance", "grass", "hell", "incident", "joke", "leg", "mate", "nerve", "pen", "priest", "promise", "resort", "roof", "sail", "script", "station", "tower", "witness", "you", "can", "many", "other", "make", "look", "being", "might", "public", "keep", "give", "local", "she", "long", "feel", "tonight", "common", "change", "past", "possible", "today", "personal", "national", "natural", "show", "check", "call", "pay", "increase", "individual", "ask", "guard", "main", "potential", "international", "cook", "following", "working", "dance", "cold", "low", "deal", "worth", "necessary", "produce", "present", "talk", "tell", "drive", "support", "remove", "run", "due", "middle", "reserve", "leave", "reach", "serve", "beautiful", "active", "negative", "stay", "visual", "cover", "rise", "white", "junior", "unique", "classic", "lift", "private", "teach", "concern", "fly", "broad", "gain", "rich", "stand", "fail", "hello", "listen", "worry", "leading", "release", "finish", "press", "secret", "spring", "wait", "deep", "flow", "objective", "touch", "chemical", "dump", "push", "eat", "formal", "kick", "pass", "remote", "treat", "abuse", "burn", "print", "sleep", "advance", "consist", "double", "equal", "hire", "join", "sensitive", "win", "claim", "drag", "guess", "pull", "soft", "wear", "wonder", "count", "doubt", "forever", "nobody", "round", "slide", "whereas", "combine", "dig", "equivalent", "hunt", "march", "smell", "survey", "adult", "crazy", "gather", "prior", "rough", "scratch", "strike", "external", "illegal", "lay", "nasty", "respond", "senior", "strain", "swim", "upper", "yellow", "crash", "fold", "grab", "miss", "quote", "resolve", "sink", "spare", "sweet", "twist", "usual", "brave", "concentrate", "grand", "mine", "quiet", "regret", "rush", "shift", "steal", "surround", "bear", "dare", "delay", "female", "inevitable", "kiss", "pop", "quit", "representative", "rip", "silly", "spell", "stupid", "temporary", "wake", "yesterday"];

  //var demoList = ["cat", "Arnold Swartzenagger", "supercalafragilisticexpealidocous", "normal", "two words"];

  var todaysWordList = nounList;

  //I Owe kyle $10 for Denny pancakes and smoothies Nov. 4 (reminder: slave laborer)

  var colorPerTimeObj = {
    0.000: colorCode['rapper3'],
    2.213: colorCode['rapper4'],
    4.093: colorCode['rapper1'],
    5.961: colorCode['rapper2'],
    7.840: colorCode['rapper3'],
    8.774: colorCode['rapper4'],
    9.720: colorCode['rapper1'],
    10.660: colorCode['rapper2'],
    11.588: colorCode['rapper3'],
    12.061: colorCode['rapper4'],
    12.533: colorCode['rapper1'],
    "13.000": colorCode['rapper2'],
    13.479: colorCode['rapper3'],
    13.707: colorCode['rapper4'],
    13.934: colorCode['rapper1'],
    14.171: colorCode['rapper2'],
    14.407: colorCode['rapper3'],
    14.641: colorCode['rapper4'],
    14.866: colorCode['rapper1'],
    19.083: colorCode['rapper2'],
    22.839: colorCode['rapper3'],
    26.589: colorCode['rapper4'],
    29.859: colorCode['rapper1'],
    37.838: colorCode['rapper2'],
    44.852: colorCode['rapper3'],
    52.834: colorCode['rapper4'],
    59.842: colorCode['rapper1'],
    74.855: colorCode['rapper2'],
    89.857: colorCode['rapper3'],
    104.852: colorCode['rapper4'],
    120.334: 'black'
  };

  var songTimes = Object.keys(colorPerTimeObj);
  var lastIntervalIndex = songTimes.length-1;
  var thisSwitch = 0;
  var thisShowsWords = [];

  var randWord = function() {
    return todaysWordList[Math.floor(todaysWordList.length*Math.random())];
  };

  $scope.eyeOpen =  "fa fa-eye";

  $scope.noneIfHideButtonPushed = {a:"white", b:"white", c:"white", d: "white", e: "white", f:"white"};

  $scope.hideWords = function() {
    if ($scope.eyeOpen === "fa fa-eye") {
      $scope.eyeOpen = "fa fa-eye-slash";
      for (var key in $scope.word) {
        if ($scope.word[key] != false)
          $scope.noneIfHideButtonPushed[key] = 'black'
      }

      $scope.buttonCheck();

    } else {
      $scope.eyeOpen = "fa fa-eye";
      for (var key in $scope.word) {
        $scope.noneIfHideButtonPushed[key] = 'white';
      }
    }
  };


  $scope.startPractice = function() {
    for (var i = 0; i < 7; i++ )
    thisShowsWords.push(randWord());
    thisShowsWords.push("");
    //console.log("number of switches", numberOfSwitches);
    //console.log("the last letter:", thisShowsWords[showWordIntervals.indexOf(numberOfSwitches)]);
    startTimer();
  };


  $scope.buttonCheck = function() {
    var words = new Array;

    for(var i in $scope.word) {
      words.push($scope.word[i]);
    }

    if (words.length === 7 && $scope.eyeOpen === "fa fa-eye-slash") {
      $scope.isStartReady = 'green';
    } else {
      //$scope.isStartReady = 'grey';
    }
  };

  //fast test (also change intervals)
  //var showWordIntervals = [1, 2, 3, 4, 5, 6, 7, lastIntervalIndex];

  var showWordIntervals = [18, 22, 24, 26, 27, 28, 29, lastIntervalIndex];

  var changeSlide = function() {
    if (thisSwitch < lastIntervalIndex) {
      var lastTime = songTimes[thisSwitch];
      thisSwitch ++;
      var thisTime = songTimes[thisSwitch];
      timerForSlideChange = setTimeout(function () {
        changeColor(thisTime);
        displayWord();
        changeSlide();
        if (showWordIntervals.indexOf(thisSwitch) === showWordIntervals.length-1) {
          $scope.isNoneOrBlock = "block";

        }
      }, thisTime * 1000 - lastTime * 1000);
    }

  };

  var changeColor = function(time) {
    $scope.$apply(function() {
      console.log("color set", colorPerTimeObj[time]);
      $scope.eachBackground = colorPerTimeObj[time];
    })
  };

  var displayWord = function()  {
    if (showWordIntervals.indexOf(thisSwitch) != -1) {
      //the index of the word will match with the index of the interval
      var whichWord = showWordIntervals.indexOf(thisSwitch);
      console.log("the word is at index:" + whichWord);
      $scope.$apply(function() {
        $scope.dependsOnWordLength = ($window.innerWidth*1.3)/thisShowsWords[whichWord].length + "px" ;
        console.log("this is the display", thisShowsWords[whichWord]);
        $scope.wordDisplay = thisShowsWords[whichWord].toUpperCase();

      })
    }
  };

  var startRapping = function() {
    console.log("song starting!");
    changeSlide();
    audio.play();
  };



  function displayNumbers() {
    if (timeCount > 1) {
      timeCount--;
      $scope.countDownDisplay = timeCount;
      console.log("seconds left:", timeCount);
    }
    else if (timeCount === 1) {
      timeCount--;
      $scope.countDownDisplay = "";
      console.log("seconds left:");
    } else {
      clearInterval(countDown);
      $scope.eachBackground = '#A95E5F';
      startRapping();
    }
  }


  var startTimer = function() {
    $scope.noneIfSongIsStarting = 'none';
    countDown = setInterval(function(){
      $scope.$apply(function(){
        displayNumbers();
      })
    }, 1000);
  };


  $scope.startShow = function() {
    //event.preventDefault();
    if(Object.keys($scope.word).length === 7 && $scope.eyeOpen === "fa fa-eye-slash") {
      console.log("This button was clicked!!");
      var wordObject = $scope.word;
      var objKeys = Object.keys(wordObject).sort();

      for (var i in objKeys) {
        thisShowsWords.push(wordObject[objKeys[i]]);
      }

      thisShowsWords.push("");
      $scope.isBlack = 'black';
      startTimer();
    }

  };


});


