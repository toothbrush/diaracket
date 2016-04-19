package fr.diaspec.webcam.implement;

import fr.diaspec.webcam.generated.AbstractMakeAd;

public class MakeAd extends AbstractMakeAd {



	@Override
	protected String whenMakeAdRequired(IPProxy localIPProxy) {
		return "Here's an advert for you, " + localIPProxy.queryIPValue() + "!";
		}

}
