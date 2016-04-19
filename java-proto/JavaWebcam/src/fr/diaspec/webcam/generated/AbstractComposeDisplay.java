package fr.diaspec.webcam.generated;

import android.graphics.Bitmap;
import fr.diaspec.webcam.common.Publisher;
import fr.diaspec.webcam.common.Subscriber;
import fr.diaspec.webcam.common.componentTypes.Context;
import fr.diaspec.webcam.common.maybe.Just;
import fr.diaspec.webcam.common.maybe.Maybe;

public abstract class AbstractComposeDisplay extends Publisher<Bitmap> implements Context, Subscriber<Bitmap>
{
  protected abstract Maybe<Bitmap> onProcessPictureProvided (Bitmap newValue, MakeAdProxy localMakeAdProxy)
  ;
  public final void trigger (Bitmap value)
  {
    MakeAdProxy localMakeAdProxy = new MakeAdProxy();
    localMakeAdProxy.setAccessible(true);
    Maybe<Bitmap> v = onProcessPictureProvided(value, localMakeAdProxy);
    localMakeAdProxy.setAccessible(false);
    if (v instanceof Just)
      notify(((Just<Bitmap>) v).just_value);
  }
  protected final class MakeAdProxy
  {
    private MakeAdProxy ()
    {
    }
    final private void setAccessible (boolean isAccessible)
    {
      this.isAccessible = isAccessible;
    }
    private boolean isAccessible = false;
    final public String queryMakeAdValue ()
    {
      if (isAccessible)
      {
        return runner.getMakeAdInstance().requireValue();
      }
      else
      {
        throw new RuntimeException("Access forbidden for MakeAd source");
      }
    }
  }
  private AbstractRunner runner;
  final protected void init (AbstractRunner runner)
  {
    this.runner = runner;
  }
}