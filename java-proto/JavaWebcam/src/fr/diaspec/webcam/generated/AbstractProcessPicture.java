package fr.diaspec.webcam.generated;

import android.graphics.Bitmap;
import fr.diaspec.webcam.common.Publisher;
import fr.diaspec.webcam.common.Subscriber;
import fr.diaspec.webcam.common.componentTypes.Context;

public abstract class AbstractProcessPicture extends Publisher<Bitmap> implements Context, Subscriber<Bitmap>
{
  protected abstract Bitmap onCameraProvided (Bitmap newValue)
  ;
  public final void trigger (Bitmap value)
  {
    Bitmap v = onCameraProvided(value);
    notify(v);
  }
  private AbstractRunner runner;
  final protected void init (AbstractRunner runner)
  {
    this.runner = runner;
  }
}