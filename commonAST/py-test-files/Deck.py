class Deck(object):
    #INITIATE DECK
    def __init__(self, card_dict):
        #LIST FOR CARDS
        self.cards = []
        #ADD CARDS FROM INPUT TO LIST
        for card in card_dict:
            self.cards.append(card_dict[card])
            
    def __str__(self):
        #INITIALIZE STRING
        string = ""
        string += "There are {} cards in the deck:\n".format(len(self.cards))
        #FOR ALL CARDS IN LIST
        for card in self.cards: 
            #ADD NAME AND TYPE
            string += card["name"] + " (" + card["type"] + "):"
            #IF THEY HAVE A MANACOST, ADD MANACOST
            if "manaCost" in card:
                string += " mana cost " + card["manaCost"] + ", "
            #IF THEY HAVE POWER, ADD POWER
            if "power" in card:
                string += "power " + card["power"] + ", "
            #IF THEY HAVE TOUGHNESS, ADD TOUGHNESS    
            if "toughness" in card:
                string += "toughness " + card["toughness"]
            #ADD BLANK    
            string += "\n"
            #IF THERE IS TEXT, ADD TEXT
            if "text" in card:
                string += card["text"]
                #ADD BLANK
            string += "\n\n"
            
        #RETURN TOTAL STRING    
        return string
            
    #ADD CARDS TO LIST        
    def add_card(self, card):
        self.cards.append(card)