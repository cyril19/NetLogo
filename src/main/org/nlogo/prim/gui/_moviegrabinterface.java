package org.nlogo.prim.gui;

import org.nlogo.api.LogoException;
import org.nlogo.nvm.EngineException;
import org.nlogo.nvm.Syntax;
import org.nlogo.window.GUIWorkspace;

public final strictfp class _moviegrabinterface
    extends org.nlogo.nvm.Command {
  @Override
  public Syntax syntax() {
    return Syntax.commandSyntax();
  }

  @Override
  public void perform(final org.nlogo.nvm.Context context)
      throws LogoException {
    if (!(workspace instanceof GUIWorkspace)) {
      throw new EngineException(
          context, this, token().name() + " can only be used in the GUI");
    }
    if (world.program().is3D) {
      throw new EngineException(
          context, this,
          token().name() + " is not supported in NetLogo 3D");
    }
    ((GUIWorkspace) workspace).updateUI();
    workspace.waitFor
        (new org.nlogo.api.CommandRunnable() {
          public void run() throws LogoException {
            try {
              if (((GUIWorkspace) workspace).movieEncoder == null) {
                throw new EngineException
                    (context, _moviegrabinterface.this,
                        "Must call MOVIE-START first");
              }
              ((GUIWorkspace) workspace)
                  .movieEncoder.add
                  (org.nlogo.awt.Utils.paintToImage
                      ((java.awt.Component)
                          ((GUIWorkspace) workspace)
                              .viewWidget.findWidgetContainer()));
            } catch (java.io.IOException ex) {
              throw new EngineException
                  (context, _moviegrabinterface.this, ex.getMessage());
            }
          }
        });
    context.ip = next;
  }
}