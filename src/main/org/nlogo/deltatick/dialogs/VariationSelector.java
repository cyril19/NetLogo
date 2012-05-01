package org.nlogo.deltatick.dialogs;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.HashMap;

import org.nlogo.deltatick.dialogs.TraitSelector;

/**
 * Created by IntelliJ IDEA.
 * User: aditi
 * Date: 10/12/11
 * Time: 12:24 AM
 * To change this template use File | Settings | File Templates.
 */
// TODO    Be able to add as many variations as wanted (with a max of ~5)
// TODO Pressing "Okay" sends values, not pressing the red cancel button
    // TODO: Add EXIT_ON_CLOSE

public class VariationSelector
        extends JDialog {
    JPanel text;
    JButton addVariation;
    JPanel buttonsPanel;
    //JButton okay;
    JLabel label;
    boolean populate;
    HashMap<String, String> numberVariation = new HashMap<String, String>();


    ArrayList<JTextField> varInputList = new ArrayList<JTextField>();
    ArrayList<String> variationList = new ArrayList<String>();
    ArrayList<JTextField> numberList = new ArrayList<JTextField>();

    private JDialog thisDialog = this;

    public VariationSelector(Frame parent) {
        super(parent, true);
        initComponents();
        this.setVisible(false);
        boolean populate = false;
    }

    public void showMe() {
        thisDialog.setVisible(true);
    }

    public void initComponents() {
        thisDialog.setSize(1000, 1000);
        JPanel text = new JPanel();
        JLabel label = new JLabel("What are the variations of this trait?");
        text.add(label);
        JLabel label1 = new JLabel();
        label1.setText("Variation");
        JLabel label2 = new JLabel();
        label2.setText("Number");
        label.setVisible(true);

        for (int i = 0; i < 6; i++) {
            varInputList.add(new JTextField(8));
        }

        for (int i = 0; i < 6; i++) {
            numberList.add(new JTextField(3));
        }

        addVariation = new JButton("Add variation");
        activateButtons();


        thisDialog.setDefaultCloseOperation(HIDE_ON_CLOSE);

        org.jdesktop.layout.GroupLayout layout = new org.jdesktop.layout.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setAutocreateContainerGaps(true);
        layout.setAutocreateGaps(true);

        layout.setHorizontalGroup(
                layout.createParallelGroup(org.jdesktop.layout.GroupLayout.LEADING)
                        .add(text)
                        .add(layout.createSequentialGroup()
                                .add(layout.createParallelGroup()
                                        .add(label1)
                                        .add(varInputList.get(0))
                                        .add(varInputList.get(1))
                                        .add(varInputList.get(2))
                                        .add(varInputList.get(3))
                                        .add(varInputList.get(4)))
                                .add(layout.createParallelGroup()
                                        .add(label2)
                                        .add(numberList.get(0))
                                        .add(numberList.get(1))
                                        .add(numberList.get(2))
                                        .add(numberList.get(3))
                                        .add(numberList.get(4)))
                                .add(layout.createSequentialGroup()
                                        .add(addVariation)
                                        //.add(okay)
                                        )
                        ));

        layout.setVerticalGroup(
                layout.createSequentialGroup()
                        .add(text)
                        .add(layout.createParallelGroup()
                                .add(label1)
                                .add(label2))
                                .add(layout.createParallelGroup()
                                        .add(varInputList.get(0))
                                        .add(numberList.get(0)))
                                        .add(layout.createParallelGroup()
                                            .add(varInputList.get(1))
                                            .add(numberList.get(1)))
                                        .add(layout.createParallelGroup()
                .add(varInputList.get(2))
                .add(numberList.get(2)))
                .add(layout.createParallelGroup()
                .add(varInputList.get(3))
                .add(numberList.get(3)))
                .add(layout.createParallelGroup()
                .add(varInputList.get(4))
                .add(numberList.get(4)))
                .add(layout.createParallelGroup()
                .add(addVariation)
                //.add(okay)
                )
                );
        pack();
    }


    public void activateButtons() {
        addVariation.addActionListener(new java.awt.event.ActionListener() {
            public void actionPerformed(java.awt.event.ActionEvent evt) {
                int i = 0;
                for ( JTextField textField : varInputList ) {
                    if (varInputList.get(i).getText().isEmpty() == false) {
                        variationList.add(varInputList.get(i).getText());
                        System.out.println("not empty");
                    }

                    i++;
                }
                populate = true;
                getVariationList();
                data();

                int j = 0;
                for ( JTextField textField : varInputList ) {
                    textField.setText("");
                    j++;
                }
                int k = 0;
                for ( JTextField textField : numberList ) {
                    textField.setText("");
                    k++;
                }
                varInputList.get(0).requestFocus();
                thisDialog.setVisible(false);
            }
        }
        );
    }

    public void varName(int i) {
        variationList.add(varInputList.get(i).getText());
    }

    public ArrayList<String> getVariationList() {
        System.out.println(variationList);
        return variationList;
    }

    public boolean check() {
        return populate;
    }

  public HashMap<String, String> data() {
        int i = 0;
        for ( JTextField textField : varInputList ) {
            if (! textField.getText().equals("")) {
            numberVariation.put( varInputList.get(i).getText(), numberList.get(i).getText() );
        }
            i++;
        }
        return numberVariation;
    }
}
