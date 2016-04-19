package fr.diaspec.webcam.generated;

import fr.diaspec.webcam.common.Publisher;
import fr.diaspec.webcam.common.componentTypes.Context;

public abstract class AbstractMakeAd extends Publisher<String> implements Context
{
  protected abstract String whenMakeAdRequired (IPProxy localIPProxy)
  ;
  protected final String requireValue ()
  {
    IPProxy localIPProxy = new IPProxy();
    localIPProxy.setAccessible(true);
    String v = whenMakeAdRequired(localIPProxy);
    localIPProxy.setAccessible(false);
    return v;
  }
  protected final class IPProxy
  {
    private IPProxy ()
    {
    }
    final private void setAccessible (boolean isAccessible)
    {
      this.isAccessible = isAccessible;
    }
    private boolean isAccessible = false;
    final public String queryIPValue ()
    {
      if (isAccessible)
      {
        return runner.getIPInstance().requireValue();
      }
      else
      {
        throw new RuntimeException("Access forbidden for IP source");
      }
    }
  }
  private AbstractRunner runner;
  final protected void init (AbstractRunner runner)
  {
    this.runner = runner;
  }
}