package fr.diaspec.webcam.generated;

import android.graphics.Bitmap;
import fr.diaspec.webcam.common.Publisher;
import fr.diaspec.webcam.common.componentTypes.Source;

public abstract class AbstractCamera extends Publisher<Bitmap> implements Source<Bitmap>
{
  protected abstract Bitmap getCameraValue ()
  ;
  public Bitmap requireValue ()
  {
    return getCameraValue();
  }
}