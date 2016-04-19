package fr.diaspec.webcam.implement;

import android.graphics.Bitmap;
import android.graphics.Canvas;
import android.graphics.Color;
import android.graphics.Paint;
import android.graphics.Rect;
import fr.diaspec.webcam.common.maybe.Just;
import fr.diaspec.webcam.common.maybe.Maybe;
import fr.diaspec.webcam.common.maybe.Nothing;
import fr.diaspec.webcam.generated.AbstractComposeDisplay;

public class ComposeDisplay extends AbstractComposeDisplay {

	@Override
	protected Maybe<Bitmap> onProcessPictureProvided(Bitmap modifiedPic,
			MakeAdProxy discover) {

		String ad = discover.queryMakeAdValue();
		if (ad == null || ad.equals("")) {
			return new Nothing<Bitmap>();
		}
		
		Bitmap dest = Bitmap.createBitmap(800, 600, Bitmap.Config.ARGB_8888);
		
		Canvas c = new Canvas(dest);
		Paint paint = new Paint();
		paint.setTextSize(24);
		
		c.drawBitmap(modifiedPic, null, new Rect(0, 0, 800, 600), paint);
		paint.setColor(Color.WHITE);
		
		c.drawText(ad, 0, 30, paint);
		
		return new Just<Bitmap>(dest);
	}

}
