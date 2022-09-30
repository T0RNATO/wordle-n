let ones = ["", "uno", "du", "tri", "tetr", "pent", "sex", "hept", "oct", "non"]
let tens = ["", "dec", "vigint", "trigint", "quadrint", "quinquagint", "sexagint", "septuagint", "octogint", "nonagint"]
let fiveLengthPossibleAnswers = ["cigar", "rebut", "sissy", "humph", "awake", "blush", "focal", "evade", "naval", "serve", "heath", "dwarf", "model", "karma", "stink", "grade", "quiet", "bench", "abate", "feign", "major", "death", "fresh", "crust", "stool", "colon", "abase", "marry", "react", "batty", "pride", "floss", "helix", "croak", "staff", "paper", "unfed", "whelp", "trawl", "outdo", "adobe", "crazy", "sower", "repay", "digit", "crate", "cluck", "spike", "mimic", "pound", "maxim", "linen", "unmet", "flesh", "booby", "forth", "first", "stand", "belly", "ivory", "seedy", "print", "yearn", "drain", "bribe", "stout", "panel", "crass", "flume", "offal", "agree", "error", "swirl", "argue", "bleed", "delta", "flick", "totem", "wooer", "front", "shrub", "parry", "biome", "lapel", "start", "greet", "goner", "golem", "lusty", "loopy", "round", "audit", "lying", "gamma", "labor", "islet", "civic", "forge", "corny", "moult", "basic", "salad", "agate", "spicy", "spray", "essay", "fjord", "spend", "kebab", "guild", "aback", "motor", "alone", "hatch", "hyper", "thumb", "dowry", "ought", "belch", "dutch", "pilot", "tweed", "comet", "jaunt", "enema", "steed", "abyss", "growl", "fling", "dozen", "boozy", "erode", "world", "gouge", "click", "briar", "great", "altar", "pulpy", "blurt", "coast", "duchy", "groin", "fixer", "group", "rogue", "badly", "smart", "pithy", "gaudy", "chill", "heron", "vodka", "finer", "surer", "radio", "rouge", "perch", "retch", "wrote", "clock", "tilde", "store", "prove", "bring", "solve", "cheat", "grime", "exult", "usher", "epoch", "triad", "break", "rhino", "viral", "conic", "masse", "sonic", "vital", "trace", "using", "peach", "champ", "baton", "brake", "pluck", "craze", "gripe", "weary", "picky", "acute", "ferry", "aside", "tapir", "troll", "unify", "rebus", "boost", "truss", "siege", "tiger", "banal", "slump", "crank", "gorge", "query", "drink", "favor", "abbey", "tangy", "panic", "solar", "shire", "proxy", "point", "robot", "prick", "wince", "crimp", "knoll", "sugar", "whack", "mount", "perky", "could", "wrung", "light", "those", "moist", "shard", "pleat", "aloft", "skill", "elder", "frame", "humor", "pause", "ulcer", "ultra", "robin", "cynic", "aroma", "caulk", "shake", "dodge", "swill", "tacit", "other", "thorn", "trove", "bloke", "vivid", "spill", "chant", "choke", "rupee", "nasty", "mourn", "ahead", "brine", "cloth", "hoard", "sweet", "month", "lapse", "watch", "today", "focus", "smelt", "tease", "cater", "movie", "saute", "allow", "renew", "their", "slosh", "purge", "chest", "depot", "epoxy", "nymph", "found", "shall", "stove", "lowly", "snout", "trope", "fewer", "shawl", "natal", "comma", "foray", "scare", "stair", "black", "squad", "royal", "chunk", "mince", "shame", "cheek", "ample", "flair", "foyer", "cargo", "oxide", "plant", "olive", "inert", "askew", "heist", "shown", "zesty", "trash", "larva", "forgo", "story", "hairy", "train", "homer", "badge", "midst", "canny", "fetus", "butch", "farce", "slung", "tipsy", "metal", "yield", "delve", "being", "scour", "glass", "gamer", "scrap", "money", "hinge", "album", "vouch", "asset", "tiara", "crept", "bayou", "atoll", "manor", "creak", "showy", "phase", "froth", "depth", "gloom", "flood", "trait", "girth", "piety", "goose", "float", "donor", "atone", "primo", "apron", "blown", "cacao", "loser", "input", "gloat", "awful", "brink", "smite", "beady", "rusty", "retro", "droll", "gawky", "hutch", "pinto", "egret", "lilac", "sever", "field", "fluff", "flack", "agape", "voice", "stead", "stalk", "berth", "madam", "night", "bland", "liver", "wedge", "augur", "roomy", "wacky", "flock", "angry", "trite", "aphid", "tryst", "midge", "power", "elope", "cinch", "motto", "stomp", "upset", "bluff", "cramp", "quart", "coyly", "youth", "rhyme", "buggy", "alien", "smear", "unfit", "patty", "cling", "glean", "label", "hunky", "khaki", "poker", "gruel", "twice", "twang", "shrug", "treat", "waste", "merit", "woven", "needy", "clown", "widow", "irony", "ruder", "gauze", "chief", "onset", "prize", "fungi", "charm", "gully", "inter", "whoop", "taunt", "leery", "class", "theme", "lofty", "tibia", "booze", "alpha", "thyme", "doubt", "parer", "chute", "stick", "trice", "alike", "recap", "saint", "glory", "grate", "admit", "brisk", "soggy", "usurp", "scald", "scorn", "leave", "twine", "sting", "bough", "marsh", "sloth", "dandy", "vigor", "howdy", "enjoy", "valid", "ionic", "equal", "floor", "catch", "spade", "stein", "exist", "quirk", "denim", "grove", "spiel", "mummy", "fault", "foggy", "flout", "carry", "sneak", "libel", "waltz", "aptly", "piney", "inept", "aloud", "photo", "dream", "stale", "unite", "snarl", "baker", "there", "glyph", "pooch", "hippy", "spell", "folly", "louse", "gulch", "vault", "godly", "threw", "fleet", "grave", "inane", "shock", "crave", "spite", "valve", "skimp", "claim", "rainy", "musty", "pique", "daddy", "quasi", "arise", "aging", "valet", "opium", "avert", "stuck", "recut", "mulch", "genre", "plume", "rifle", "count", "incur", "total", "wrest", "mocha", "deter", "study", "lover", "safer", "rivet", "funny", "smoke", "mound", "undue", "sedan", "pagan", "swine", "guile", "gusty", "equip", "tough", "canoe", "chaos", "covet", "human", "udder", "lunch", "blast", "stray", "manga", "melee", "lefty", "quick", "paste", "given", "octet", "risen", "groan", "leaky", "grind", "carve", "loose", "sadly", "spilt", "apple", "slack", "honey", "final", "sheen", "eerie", "minty", "slick", "derby", "wharf", "spelt", "coach", "erupt", "singe", "price", "spawn", "fairy", "jiffy", "filmy", "stack", "chose", "sleep", "ardor", "nanny", "niece", "woozy", "handy", "grace", "ditto", "stank", "cream", "usual", "diode", "valor", "angle", "ninja", "muddy", "chase", "reply", "prone", "spoil", "heart", "shade", "diner", "arson", "onion", "sleet", "dowel", "couch", "palsy", "bowel", "smile", "evoke", "creek", "lance", "eagle", "idiot", "siren", "built", "embed", "award", "dross", "annul", "goody", "frown", "patio", "laden", "humid", "elite", "lymph", "edify", "might", "reset", "visit", "gusto", "purse", "vapor", "crock", "write", "sunny", "loath", "chaff", "slide", "queer", "venom", "stamp", "sorry", "still", "acorn", "aping", "pushy", "tamer", "hater", "mania", "awoke", "brawn", "swift", "exile", "birch", "lucky", "freer", "risky", "ghost", "plier", "lunar", "winch", "snare", "nurse", "house", "borax", "nicer", "lurch", "exalt", "about", "savvy", "toxin", "tunic", "pried", "inlay", "chump", "lanky", "cress", "eater", "elude", "cycle", "kitty", "boule", "moron", "tenet", "place", "lobby", "plush", "vigil", "index", "blink", "clung", "qualm", "croup", "clink", "juicy", "stage", "decay", "nerve", "flier", "shaft", "crook", "clean", "china", "ridge", "vowel", "gnome", "snuck", "icing", "spiny", "rigor", "snail", "flown", "rabid", "prose", "thank", "poppy", "budge", "fiber", "moldy", "dowdy", "kneel", "track", "caddy", "quell", "dumpy", "paler", "swore", "rebar", "scuba", "splat", "flyer", "horny", "mason", "doing", "ozone", "amply", "molar", "olety", "beset", "queue", "cliff", "magic", "truce", "sport", "fritz", "edict", "twirl", "verse", "llama", "eaten", "range", "whisk", "hovel", "rehab", "macaw", "sigma", "spout", "verve", "sushi", "dying", "fetid", "brain", "buddy", "thump", "scion", "candy", "chord", "basin", "march", "crowd", "arbor", "gayly", "musky", "stain", "dally", "bless", "bravo", "stung", "title", "ruler", "kiosk", "blond", "ennui", "layer", "fluid", "tatty", "score", "cutie", "zebra", "barge", "matey", "bluer", "aider", "shook", "river", "privy", "betel", "frisk", "bongo", "begun", "azure", "weave", "genie", "sound", "glove", "braid", "scope", "wryly", "rover", "assay", "ocean", "bloom", "irate", "later", "woken", "silky", "wreck", "dwelt", "slate", "smack", "solid", "amaze", "hazel", "wrist", "jolly", "globe", "flint", "rouse", "civil", "vista", "relax", "cover", "alive", "beech", "jetty", "bliss", "vocal", "often", "dolly", "eight", "joker", "since", "event", "ensue", "shunt", "diver", "poser", "worst", "sweep", "alley", "creed", "anime", "leafy", "bosom", "dunce", "stare", "pudgy", "waive", "choir", "stood", "spoke", "outgo", "delay", "bilge", "ideal", "clasp", "seize", "hotly", "laugh", "sieve", "block", "meant", "grape", "noose", "hardy", "shied", "drawl", "daisy", "putty", "strut", "burnt", "tulip", "crick", "idyll", "vixen", "furor", "geeky", "cough", "naive", "shoal", "stork", "bathe", "aunty", "check", "prime", "brass", "outer", "furry", "razor", "elect", "evict", "imply", "demur", "quota", "haven", "cavil", "swear", "crump", "dough", "gavel", "wagon", "salon", "nudge", "harem", "pitch", "sworn", "pupil", "excel", "stony", "cabin", "unzip", "queen", "trout", "polyp", "earth", "storm", "until", "taper", "enter", "child", "adopt", "minor", "fatty", "husky", "brave", "filet", "slime", "glint", "tread", "steal", "regal", "guest", "every", "murky", "share", "spore", "hoist", "buxom", "inner", "otter", "dimly", "level", "sumac", "donut", "stilt", "arena", "sheet", "scrub", "fancy", "slimy", "pearl", "silly", "porch", "dingo", "sepia", "amble", "shady", "bread", "friar", "reign", "dairy", "quill", "cross", "brood", "tuber", "shear", "posit", "blank", "villa", "shank", "piggy", "freak", "which", "among", "fecal", "shell", "would", "algae", "large", "rabbi", "agony", "amuse", "bushy", "copse", "swoon", "knife", "pouch", "ascot", "plane", "crown", "urban", "snide", "relay", "abide", "viola", "rajah", "straw", "dilly", "crash", "amass", "third", "trick", "tutor", "woody", "blurb", "grief", "disco", "where", "sassy", "beach", "sauna", "comic", "clued", "creep", "caste", "graze", "snuff", "frock", "gonad", "drunk", "prong", "lurid", "steel", "halve", "buyer", "vinyl", "utile", "smell", "adage", "worry", "tasty", "local", "trade", "finch", "ashen", "modal", "gaunt", "clove", "enact", "adorn", "roast", "speck", "sheik", "missy", "grunt", "snoop", "party", "touch", "mafia", "emcee", "array", "south", "vapid", "jelly", "skulk", "angst", "tubal", "lower", "crest", "sweat", "cyber", "adore", "tardy", "swami", "notch", "groom", "roach", "hitch", "young", "align", "ready", "frond", "strap", "puree", "realm", "venue", "swarm", "offer", "seven", "dryer", "diary", "dryly", "drank", "acrid", "heady", "theta", "junto", "pixie", "quoth", "bonus", "shalt", "penne", "amend", "datum", "build", "piano", "shelf", "lodge", "suing", "rearm", "coral", "ramen", "worth", "psalm", "infer", "overt", "mayor", "ovoid", "glide", "usage", "poise", "randy", "chuck", "prank", "fishy", "tooth", "ether", "drove", "idler", "swath", "stint", "while", "begat", "apply", "slang", "tarot", "radar", "credo", "aware", "canon", "shift", "timer", "bylaw", "serum", "three", "steak", "iliac", "shirk", "blunt", "puppy", "penal", "joist", "bunny", "shape", "beget", "wheel", "adept", "stunt", "stole", "topaz", "chore", "fluke", "afoot", "bloat", "bully", "dense", "caper", "sneer", "boxer", "jumbo", "lunge", "space", "avail", "short", "slurp", "loyal", "flirt", "pizza", "conch", "tempo", "droop", "plate", "bible", "plunk", "afoul", "savoy", "steep", "agile", "stake", "dwell", "knave", "beard", "arose", "motif", "smash", "broil", "glare", "shove", "baggy", "mammy", "swamp", "along", "rugby", "wager", "quack", "squat", "snaky", "debit", "mange", "skate", "ninth", "joust", "tramp", "spurn", "medal", "micro", "rebel", "flank", "learn", "nadir", "maple", "comfy", "remit", "gruff", "ester", "least", "mogul", "fetch", "cause", "oaken", "aglow", "meaty", "gaffe", "shyly", "racer", "prowl", "thief", "stern", "poesy", "rocky", "tweet", "waist", "spire", "grope", "havoc", "patsy", "truly", "forty", "deity", "uncle", "swish", "giver", "preen", "bevel", "lemur", "draft", "slope", "annoy", "lingo", "bleak", "ditty", "curly", "cedar", "dirge", "grown", "horde", "drool", "shuck", "crypt", "cumin", "stock", "gravy", "locus", "wider", "breed", "quite", "chafe", "cache", "blimp", "deign", "fiend", "logic", "cheap", "elide", "rigid", "false", "renal", "pence", "rowdy", "shoot", "blaze", "envoy", "posse", "brief", "never", "abort", "mouse", "mucky", "sulky", "fiery", "media", "trunk", "yeast", "clear", "skunk", "scalp", "bitty", "cider", "koala", "duvet", "segue", "creme", "super", "grill", "after", "owner", "ember", "reach", "nobly", "empty", "speed", "gipsy", "recur", "smock", "dread", "merge", "burst", "kappa", "amity", "shaky", "hover", "carol", "snort", "synod", "faint", "haunt", "flour", "chair", "detox", "shrew", "tense", "plied", "quark", "burly", "novel", "waxen", "stoic", "jerky", "blitz", "beefy", "lyric", "hussy", "towel", "quilt", "below", "bingo", "wispy", "brash", "scone", "toast", "easel", "saucy", "value", "spice", "honor", "route", "sharp", "bawdy", "radii", "skull", "phony", "issue", "lager", "swell", "urine", "gassy", "trial", "flora", "upper", "latch", "wight", "brick", "retry", "holly", "decal", "grass", "shack", "dogma", "mover", "defer", "sober", "optic", "crier", "vying", "nomad", "flute", "hippo", "shark", "drier", "obese", "bugle", "tawny", "chalk", "feast", "ruddy", "pedal", "scarf", "cruel", "bleat", "tidal", "slush", "semen", "windy", "dusty", "sally", "igloo", "nerdy", "jewel", "shone", "whale", "hymen", "abuse", "fugue", "elbow", "crumb", "pansy", "welsh", "syrup", "terse", "suave", "gamut", "swung", "drake", "freed", "afire", "shirt", "grout", "oddly", "tithe", "plaid", "dummy", "broom", "blind", "torch", "enemy", "again", "tying", "pesky", "alter", "gazer", "noble", "ethos", "bride", "extol", "decor", "hobby", "beast", "idiom", "utter", "these", "sixth", "alarm", "erase", "elegy", "spunk", "piper", "scaly", "scold", "hefty", "chick", "sooty", "canal", "whiny", "slash", "quake", "joint", "swept", "prude", "heavy", "wield", "femme", "lasso", "maize", "shale", "screw", "spree", "smoky", "whiff", "scent", "glade", "spent", "prism", "stoke", "riper", "orbit", "cocoa", "guilt", "humus", "shush", "table", "smirk", "wrong", "noisy", "alert", "shiny", "elate", "resin", "whole", "hunch", "pixel", "polar", "hotel", "sword", "cleat", "mango", "rumba", "puffy", "filly", "billy", "leash", "clout", "dance", "ovate", "facet", "chili", "paint", "liner", "curio", "salty", "audio", "snake", "fable", "cloak", "navel", "spurt", "pesto", "balmy", "flash", "unwed", "early", "churn", "weedy", "stump", "lease", "witty", "wimpy", "spoof", "saner", "blend", "salsa", "thick", "warty", "manic", "blare", "squib", "spoon", "probe", "crepe", "knack", "force", "debut", "order", "haste", "teeth", "agent", "widen", "icily", "slice", "ingot", "clash", "juror", "blood", "abode", "throw", "unity", "pivot", "slept", "troop", "spare", "sewer", "parse", "morph", "cacti", "tacky", "spool", "demon", "moody", "annex", "begin", "fuzzy", "patch", "water", "lumpy", "admin", "omega", "limit", "tabby", "macho", "aisle", "skiff", "basis", "plank", "verge", "botch", "crawl", "lousy", "slain", "cubic", "raise", "wrack", "guide", "foist", "cameo", "under", "actor", "revue", "fraud", "harpy", "scoop", "climb", "refer", "olden", "clerk", "debar", "tally", "ethic", "cairn", "tulle", "ghoul", "hilly", "crude", "apart", "scale", "older", "plain", "sperm", "briny", "abbot", "rerun", "quest", "crisp", "bound", "befit", "drawn", "suite", "itchy", "cheer", "bagel", "guess", "broad", "axiom", "chard", "caput", "leant", "harsh", "curse", "proud", "swing", "opine", "taste", "lupus", "gumbo", "miner", "green", "chasm", "lipid", "topic", "armor", "brush", "crane", "mural", "abled", "habit", "bossy", "maker", "dusky", "dizzy", "lithe", "brook", "jazzy", "fifty", "sense", "giant", "surly", "legal", "fatal", "flunk", "began", "prune", "small", "slant", "scoff", "torus", "ninny", "covey", "viper", "taken", "moral", "vogue", "owing", "token", "entry", "booth", "voter", "chide", "elfin", "ebony", "neigh", "minim", "melon", "kneed", "decoy", "voila", "ankle", "arrow", "mushy", "tribe", "cease", "eager", "birth", "graph", "odder", "terra", "weird", "tried", "clack", "color", "rough", "weigh", "uncut", "ladle", "strip", "craft", "minus", "dicey", "titan", "lucid", "vicar", "dress", "ditch", "gypsy", "pasta", "taffy", "flame", "swoop", "aloof", "sight", "broke", "teary", "chart", "sixty", "wordy", "sheer", "leper", "nosey", "bulge", "savor", "clamp", "funky", "foamy", "toxic", "brand", "plumb", "dingy", "butte", "drill", "tripe", "bicep", "tenor", "krill", "worse", "drama", "hyena", "think", "ratio", "cobra", "basil", "scrum", "bused", "phone", "court", "camel", "proof", "heard", "angel", "petal", "pouty", "throb", "maybe", "fetal", "sprig", "spine", "shout", "cadet", "macro", "dodgy", "satyr", "rarer", "binge", "trend", "nutty", "leapt", "amiss", "split", "myrrh", "width", "sonar", "tower", "baron", "fever", "waver", "spark", "belie", "sloop", "expel", "smote", "baler", "above", "north", "wafer", "scant", "frill", "awash", "snack", "scowl", "frail", "drift", "limbo", "fence", "motel", "ounce", "wreak", "revel", "talon", "prior", "knelt", "cello", "flake", "debug", "anode", "crime", "salve", "scout", "imbue", "pinky", "stave", "vague", "chock", "fight", "video", "stone", "teach", "cleft", "frost", "prawn", "booty", "twist", "apnea", "stiff", "plaza", "ledge", "tweak", "board", "grant", "medic", "bacon", "cable", "brawl", "slunk", "raspy", "forum", "drone", "women", "mucus", "boast", "toddy", "coven", "tumor", "truer", "wrath", "stall", "steam", "axial", "purer", "daily", "trail", "niche", "mealy", "juice", "nylon", "plump", "merry", "flail", "papal", "wheat", "berry", "cower", "erect", "brute", "leggy", "snipe", "sinew", "skier", "penny", "jumpy", "rally", "umbra", "scary", "modem", "gross", "avian", "greed", "satin", "tonic", "parka", "sniff", "livid", "stark", "trump", "giddy", "reuse", "taboo", "avoid", "quote", "devil", "liken", "gloss", "gayer", "beret", "noise", "gland", "dealt", "sling", "rumor", "opera", "thigh", "tonga", "flare", "wound", "white", "bulky", "etude", "horse", "circa", "paddy", "inbox", "fizzy", "grain", "exert", "surge", "gleam", "belle", "salvo", "crush", "fruit", "sappy", "taker", "tract", "ovine", "spiky", "frank", "reedy", "filth", "spasm", "heave", "mambo", "right", "clank", "trust", "lumen", "borne", "spook", "sauce", "amber", "lathe", "carat", "corer", "dirty", "slyly", "affix", "alloy", "taint", "sheep", "kinky", "wooly", "mauve", "flung", "yacht", "fried", "quail", "brunt", "grimy", "curvy", "cagey", "rinse", "deuce", "state", "grasp", "milky", "bison", "graft", "sandy", "baste", "flask", "hedge", "girly", "swash", "boney", "coupe", "endow", "abhor", "welch", "blade", "tight", "geese", "miser", "mirth", "cloud", "cabal", "leech", "close", "tenth", "pecan", "droit", "grail", "clone", "guise", "ralph", "tango", "biddy", "smith", "mower", "payee", "serif", "drape", "fifth", "spank", "glaze", "allot", "truck", "kayak", "virus", "testy", "tepee", "fully", "zonal", "metro", "curry", "grand", "banjo", "axion", "bezel", "occur", "chain", "nasal", "gooey", "filer", "brace", "allay", "pubic", "raven", "plead", "gnash", "flaky", "munch", "dully", "eking", "thing", "slink", "hurry", "theft", "shorn", "pygmy", "ranch", "wring", "lemon", "shore", "mamma", "froze", "newer", "style", "moose", "antic", "drown", "vegan", "chess", "guppy", "union", "lever", "lorry", "image", "cabby", "druid", "exact", "truth", "dopey", "spear", "cried", "chime", "crony", "stunk", "timid", "batch", "gauge", "rotor", "crack", "curve", "latte", "witch", "bunch", "repel", "anvil", "soapy", "meter", "broth", "madly", "dried", "scene", "known", "magma", "roost", "woman", "thong", "punch", "pasty", "downy", "knead", "whirl", "rapid", "clang", "anger", "drive", "goofy", "email", "music", "stuff", "bleep", "rider", "mecca", "folio", "setup", "verso", "quash", "fauna", "gummy", "happy", "newly", "fussy", "relic", "guava", "ratty", "fudge", "femur", "chirp", "forte", "alibi", "whine", "petty", "golly", "plait", "fleck", "felon", "gourd", "brown", "thrum", "ficus", "stash", "decry", "wiser", "junta", "visor", "daunt", "scree", "impel", "await", "press", "whose", "turbo", "stoop", "speak", "mangy", "eying", "inlet", "crone", "pulse", "mossy", "staid", "hence", "pinch", "teddy", "sully", "snore", "ripen", "snowy", "attic", "going", "leach", "mouth", "hound", "clump", "tonal", "bigot", "peril", "piece", "blame", "haute", "spied", "undid", "intro", "basal", "shine", "gecko", "rodeo", "guard", "steer", "loamy", "scamp", "scram", "manly", "hello", "vaunt", "organ", "feral", "knock", "extra", "condo", "adapt", "willy", "polka", "rayon", "skirt", "faith", "torso", "match", "mercy", "tepid", "sleek", "riser", "twixt", "peace", "flush", "catty", "login", "eject", "roger", "rival", "untie", "refit", "aorta", "adult", "judge", "rower", "artsy", "rural", "shave", "bobby", "eclat", "fella", "gaily", "harry", "hasty", "hydro", "liege", "octal", "ombre", "payer", "sooth", "unset", "unlit", "vomit", "fanny"]

setTimeout(() => { window.scrollTo(0, 0); }, 200);
const $ = a => document.querySelector(a);
const $$ = a => document.querySelectorAll(a);
window.location.hash = "";

let gamename, gamemode, wordLength;
let dictionary = {3: null, 4: null, 5: null, 6: null, 7: null};
let totalPossibleGuesses = 0;

let guessesUsed = 0;
let answers = [];
let guessedWordsGuessNums = [];

let currentlyOpenMenu = undefined;

function openMenu(type) {
    const popup = $(`#${type}`);
    currentlyOpenMenu = type;
    window.addEventListener("keyup", (ev) => {
        if (ev.key == "Escape") {
            closeMenu(popup)
        }
    });
    switch (type) {
        case "win":
            popup.querySelector(".popup_content").innerText = `You won ${gamemode} ${gamename} in ${guessesUsed} turn${guessesUsed > 1 ? "s" : ""}!
                                                                \n(Out of ${totalPossibleGuesses + 1} possible turns)
                                                                \nGuesses: ${guessedWordsGuessNums.join(", ")}`;
            break;
        case "lose":
            popup.querySelector(".popup_content").innerText = `You lost ${gamemode} ${gamename}. Better luck next time!`; break;
    }
    popup.classList.add("anim");
    setTimeout(() => {
        popup.classList.add("anim2");
    }, 10);
}

function closeMenu(popup) {
    popup.classList.remove("anim2");
    setTimeout(() => {
        popup.classList.remove("anim");
    }, 250);
    currentlyOpenMenu = undefined;
    window.removeEventListener("keyup", (ev) => {
        if (ev.key == "Escape") {
            closeMenu(popup)
        }
    });
}

function keyPress(key) {
    let cells = $$("td.open")
    let i = 0
    if (key.match(/([a-z]|[A-Z])/g) && guess.length < wordLength && key.length == 1) {
        cells.forEach((cell) => {
            if (i % wordLength == cursor) {
                cell.innerText = key;
                cell.classList.add("animate");
            }
            i++
        })
        guess += key;
        cursor++
        if (cursor == wordLength && !dictionary[wordLength].includes(guess)) {
            cells.forEach((cell) => {
                cell.style.color = "red";
            })
        }
    } else if (key.toLowerCase() == "backspace" && guess.length > 0) {
        cursor--
        cells.forEach((cell) => {
            cell.style.color = "unset";
            if (i % wordLength == cursor) {
                cell.innerText = "";
                cell.classList.remove("animate");
            }
            i++
        })
        guess = guess.slice(0, -1)
    } else if (key.toLowerCase() == "enter" && guess.length == wordLength) { // if enter pressed
        if (dictionary[wordLength].includes(guess)) { // if the guess is legal
            guessesUsed++;
            $$("tr.open").forEach((tr) => {
                tr.classList.remove("open");
                tr.classList.add("clue");
            })
            $$("table").forEach((table, index) => { // loop through tables
                if (!table.classList.contains("completed")) {
                    markWord(guess, answers[index], table) // mark the guess against each table's answer
                    table.querySelectorAll("td.empty").forEach((cell, i) => { // loop through the empty cells of the table,
                        if (i < wordLength && guess != answers[index]) { // get the first five,
                            cell.classList.add("open"); // and update their classes
                            cell.classList.remove("empty");
                        }
                    })
                    if (guess == answers[index]) { // if the guess was right for this table,
                        table.classList.add("completed"); // mark it as completed
                        answers[index] = "";
                        guessedWordsGuessNums[index] = guessesUsed;
                    } else if (totalPossibleGuesses - guessesUsed < 0) {
                        openMenu("lose");
                    } else {
                        table.querySelector("tr.empty").classList.add("open");
                        table.querySelector("tr.empty").classList.remove("empty");
                    }
                }  
            })
            guess.split('').forEach((letter) => { // grey out used letters on keyboard
                $("#" + letter).classList.add('used');
                if (getSetting("greyout") == "used") {
                    $("#" + letter).classList.add('u');
                }
            })
            $$(".used").forEach((el) => {
                if (answers.every(e => !e.includes(el.innerText.toLowerCase()))) {
                    el.classList.add('notinword');
                    if (getSetting("greyout") == "notinwords") {
                        el.classList.add('n');
                    }
                }
            })
            cells.forEach((cell) => {
                cell.classList.remove("open")
                cell.classList.add("clue")
            })
            guess = ""
            cursor = 0
            if ($$("table:not(.completed)").length == 0) {
                openMenu("win");
            }
            $$("tr.open+tr.empty td:last-child").forEach((el) => {
                el.setAttribute("data-guesses-left", totalPossibleGuesses - guessesUsed);
            })
            
        } else {
            cells.forEach((cell) => {
                cell.innerText = "";
                cursor = 0;
                cell.style.color = "unset";
            })
            guess = "";
        }
    }
}

function updatebuttons() {
    let prac = $("#practice");
    let daily = $("#daily");
    if (gamename == "d") {
        prac.innerText = "Play practice";
        daily.innerText = "Play daily";
        prac.disabled = true;
        daily.disabled = true;
    } else {
        prac.innerText = "Play a practice " + gamename;
        daily.innerText = "Play the daily " + gamename;
        prac.disabled = false;
        daily.disabled = false;
    }
}

function isValidBoard(str) {
    return /^0{0,}([1-9]|[1-9][0-9]|100)$/.test(str)
}

$("input").addEventListener("input", () => {
    let numOfBoards = $("input").value
    while( numOfBoards.charAt(0) == "0" ) numOfBoards = numOfBoards.substring(1);
    const one = Number(numOfBoards.charAt(1))
    const ten = Number(numOfBoards.charAt(0))
    if (numOfBoards == "1") {
        gamename = "Wordle";
    } else if (numOfBoards == "100") {
        gamename = "Centordle";
    } else if (!isValidBoard(numOfBoards)) {
        gamename = "d";
    } else {
        if (numOfBoards.length == 2) {
            let wordle = ones[one] + (one > 3 ? "a" : "") + tens[ten] + "ordle"
            gamename = wordle.charAt(0).toUpperCase() + wordle.slice(1)
        } else if (numOfBoards.length == 1){
            gamename = ones[numOfBoards].charAt(0).toUpperCase() + ones[numOfBoards].slice(1) + "ordle"
        }
    }
    updatebuttons()
})

let cancel_event = false;

function backbutton(type) {
    if (cancel_event) {
        cancel_event = false;
        return;
    } else {
        if (confirm("This will delete your progress - are you sure you want to go back?")) {
            window.removeEventListener("hashchange", backbutton);
            window.location.reload()
        } else {
            if (type == "browser_button") {
                cancel_event = true;
            }
            window.location.hash = "game";
        }
    }
}

function play(type) {
    const numOfBoards = Number($("input").value);
    wordLength = Number($("#wordlength").value);
    if (isValidBoard(String(numOfBoards))) {
        window.location.hash = "game";
        setTimeout(() => {
            window.addEventListener("hashchange", () => {backbutton("browser_button")});
        }, 10);
        if (type == "daily") {
            Math.seedrandom(Math.floor((((Date.now() / 1000) / 60) / 60) / 24) + numOfBoards);
            gamemode = "the daily";
        } else if (type == "practice") {
            gamemode = "a practice";
        }
        guessedWordsGuessNums = new Array(numOfBoards)
        totalPossibleGuesses = numOfBoards + 5 - (5 - wordLength);
        $("#subtitle").classList.add("anim");
        $("#start").classList.add("anim");
        $("#backbutton").classList.add("game");
        $("#gamediv").innerHTML = ("<table class='game'>" + ("<tr class='open'>" + "<td class='open'></td>".repeat(wordLength) + "</tr>") + ("<tr class='empty'>" + "<td class='empty'></td>".repeat(wordLength) + "</tr>").repeat(totalPossibleGuesses) + "</table>").repeat(numOfBoards);
        let dictLength = dictionary[wordLength].length;
        $$("table").forEach((el, index) => {
            el.id = index;
            if (wordLength == 5) {
                answers.push(fiveLengthPossibleAnswers[Math.floor(Math.random() * 2309)]);
            } else {
                answers.push(dictionary[wordLength][Math.floor(Math.random() * dictLength)])
            }
        })
        document.body.classList.add("anim");
        $("#gamediv").classList.add("anim");
        console.log(answers)
        document.body.addEventListener('keydown', (event) => {
            keyPress(event.key);
        })
        $$("tr.open+tr.empty td:last-child").forEach((el) => {
            el.setAttribute("data-guesses-left", totalPossibleGuesses - guessesUsed);
        })
        if (getSetting("collapse") == "collapse") {
            $$("tr").forEach((el) => {
                el.classList.add("c");
            })
    }
    }
}

function updateSetting(setting, value) {
    document.cookie = `${setting}=${value}; expires=Tue, 19 Jan 2038 04:14:07 GMT`; // update saved setting
    $(`#${setting}`).value = value; // update setting in menu
    switch (setting) {
        case "collapse": // if the setting changed is grid-collapse
            if (value == "collapse") {
                $$("tr").forEach((el) => { // loop through table rows
                    el.classList.add("c");
                })
            } else if (value == "show") {
                $$("tr").forEach((el) => { // loop through table rows
                    el.classList.remove("c");
                })
            }
            break;
        case "keyboard_pos": // if the setting changed is the keyboard position
            let kb = $("#keyboard") // update keyboard position
            kb.classList.remove("left");
            kb.classList.remove("right");
            kb.classList.remove("center");
            kb.classList.remove("none")
            kb.classList.add(value);
            break;
        case "greyout":
            switch (value) {
                case "used":
                    $$(".key.used").forEach((el) => { // loop through table rows
                        el.classList.add("u");
                        el.classList.remove("n");
                    })
                    break;
                case "notinwords":
                    $$(".key.used").forEach((el) => { // loop through table rows
                        el.classList.remove("u");
                    })
                    $$(".key.used.notinword").forEach((el) => { // loop through table rows
                        el.classList.add("n");
                    })
                    break;
                case "none":
                    $$(".key.used").forEach((el) => { // loop through table rows
                        el.classList.remove("n");
                        el.classList.remove("u");
                    })
                    break;
            }
            break;
        case "theme":
            document.firstChild.classList.value = "";
            document.firstChild.classList.add(value);
            let themes = $("#theme").dataset.allowed.split(" ");
            $$("td").forEach(el => {
                themes.forEach(theme => {
                    el.classList.remove(theme);
                })
                el.classList.add(value);
            })
            break;
    }
}

function getSetting(setting) {
    if (document.cookie.includes(setting)) { // if setting has a value
        let cookies = document.cookie.replace(/\s+/g, '').split(";"); // get array of cookies
        let value = cookies.find(row => row.startsWith(`${setting}=`)).split('=')[1]; // get saved value of the setting
        if ($(`select[data-settingname='${setting}']`).dataset.allowed.includes(value)) { // check if the saved value is legal
            return value; // return the saved value
        } else { // if setting is not legal,
            updateSetting(setting, defaultsettings[setting]); // set it to the default
            return defaultsettings[setting]; // and return that
        }
    } else { // else if setting has no value
        updateSetting(setting, defaultsettings[setting]); // set it to the default
        return defaultsettings[setting]; // and return that
    }
}

let settings = {};
let defaultsettings = { 
                        "collapse": "collapse",
                        "keyboard_pos": "left",
                        "greyout": "used",
                        "theme": "default"
                      };

for (let setting of Object.keys(defaultsettings)) { // loop through settings
    updateSetting(setting, getSetting(setting)); // create cookie if it doesn't exist
    $(`select[data-settingname='${setting}']`).addEventListener("input", (e) => { // add an event listener to each settings dropdown
        updateSetting(e.target.dataset.settingname, e.target.value); // and when it changes, update the respective setting
    })
}

$("#daily").addEventListener("click", () => {play("daily")});
$("#practice").addEventListener("click", () => {play("practice")});

$$("span.close").forEach((el) => { // popup close buttons
    el.addEventListener("click", (e) => { // add an event listener to each button
        closeMenu(e.target.parentElement); // and when it is clicked, close the menu
    })
})
$("#settingsbutton").addEventListener("click", () => {openMenu("settings")});
$("#backbutton").addEventListener("click", () => {backbutton("page_button")});

$("#start").addEventListener("transitionend", (ev) => {
    if (ev.target == $("#start")) {
        $("#keyboard").classList.add("anim")
        $("#start").remove()
        $("#subtitle").innerText = gamename;
    }
})

function markWord(guess, answer, table) {
    let cells = table.querySelectorAll("td.open");
    let greens = [];
        let nonGreens = []; // List of characters that still need to be processed
        for (let i = 0; i < wordLength; i++) { // Loop through chars of guess
            if (guess.charAt(i) == answer.charAt(i)) { // If the char is in the right place and letter
                col(cells[i], "green"); // Make it green
                greens.push(guess.charAt(i)); // And add it to the list of greens
                nonGreens.push("");
            } else {
                nonGreens.push(guess.charAt(i)); // Add it to the list of un-processed letters
            }
        }
        let yellows = [];
        for (let i = 0; i < wordLength; i++) { // Loop through chars again
            let c = nonGreens[i]; // Current char
            if (c == "") {continue} // If empty, skip to the next repetition
            if (answer.includes(c) && c != answer.charAt(i)) { // If the char is in the word, but in the wrong place
                if (greens.includes(c)) { // If the char is a duplicate of a correct letter
                    if (answer.split(c).length > 2 // If there's more than 2 of the char...
                     && yellows.concat(greens).filter(x => x==c).length < answer.split(c).length - 1) { // And the number of the char already processed is smaller than the number of the char in the actual word
                        col(cells[i], "yellow") // Set the cell to yellow
                        yellows.push(c) // Add the char to the list of yellow cells
                    } else {
                        col(cells[i], "grey") // Else, make it grey
                    }
                } else if (yellows.concat(greens).filter(x => x==c).length < answer.split(c).length - 1) { // If the number of the char already processed is smaller than the number of the char in the actual word
                    col(cells[i], "yellow") // Set the cell to yellow
                    yellows.push(c) // Add the char to the list of yellow cells
                } else {
                    col(cells[i], "grey") // Else, make it grey
                }
            } else if (!answer.includes(c)) {
                col(cells[i], "grey") // If the char is not in the answer, make the cell grey
            }
        }
}

let cursor = 0
let guess = ''
$$(".key").forEach((el) => {
    el.addEventListener("click", (event) => {
        keyPress(event.target.id.toLowerCase());
    })
})

function col(el, colour) { // Function to set the bg colour of a cell
    el.classList.add(colour);
}

window.addEventListener("load", () => {
    fetch("https://raw.githubusercontent.com/derekchuank/high-frequency-vocabulary/master/20k.txt").then((response) => response.text()).then((data) => {
        dict = data.split(/\r?\t?\n/);
        for (let i = 3; i <= 7; i++) {
            dictionary[i] = dict.filter(word => word.length == i)
        }
        console.log("Dictionary Loaded")
    });
})

$("#wordlength").addEventListener("change", () => {
    $("#warning").classList.add("display");
})