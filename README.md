# Value of Information/Discrimination
A repository containing the codebase for calculating VoI and VoD. 

**MAJOR UPDATE - MAY 10, 2023**: The codebase has been updated to reflect advice from Benjamin Tereick and internal discussion. A rewrite of this README is forthcoming, but please see this new [formula sheet](https://docs.google.com/document/d/1DIiKe-mjDBL3iz326DHGS8vUODi-Pbq2Q5sjAJl2E6c/edit#) to see new iterations of VoI/VoD.

----

Below text adapted from Page's [VoI/VoD explainer](https://docs.google.com/document/d/16JchstHAuZY0iMxuYgWIMqBHdm4RBrY5mzPv64SPNkc/edit#).

## Value of Information

The first candidate metric is value of information (VoI). The VoI of a crux is the amount one would update their forecast on the ultimate question if they knew the answer to the crux. Formally:

> VoI = |(P(U)-P(U|C)|*P(C) + |P(U)-P(U|¬C)|*P(¬C)

Consider an example involving a hypothetical forecaster, Alice, and the following ultimate question and cruxes:

- U: Will two or more countries exchange nuclear strikes in the next decade?
- C1: Will Iran produce enough highly enriched uranium for a nuclear weapon core by December 31, 2020?
- C2: Will Iran develop an intercontinental ballistic missile capable of carrying a nuclear warhead by December 31, 2022?

Alice believes there is a 20% chance that two or more countries exchange nuclear strikes in the next decade. How relevant would knowing the answers to C1 or C2 be to Alice’s P(U)? The table below shows the conditional probabilities and the VoI for C1 and C2.

|  | P(U) | P(C) | P(U\|C) | P(U\|¬C) | VoI |
|:---:|:---:|:---:|:---:|:---:|:---:|
| C1 | 20% | 50% | 23% | 17% | 3% |
| C2 | 20% | 10% | 29% | 19% | 1.8% |

As you can see, C1 has higher VoI than C2: If Alice knew the answer to C1, her P(U) would change by 3 percentage points in expectation, either from 20% to 23% (50% chance) or from 20% to 17% (50% chance). The largest update to Alice’s P(U) would occur if she learned that C2 would occur, but because C2 is unlikely to occur (10% chance), the value of that update must be tempered accordingly.  

VoI works at the individual level. For any forecaster with a view on P(U), we can evaluate candidate cruxes with respect to their VoI for U. 

VoI also works at the group level. For any group of forecasters with an aggregate view on P(U), we can evaluate candidate cruxes questions with respect to their collective VoI for U.

## Value of Discrimination
But VoI ceases to work at the group level if the members of the group have a very different understanding of what drivers increase or decrease P(U). For example, some forecasters might believe that Iran’s ability to create highly enriched uranium is an important driver of nuclear war because they believe nuclear deterrence is fragile. Whereas other forecasters might believe that Iran’s ability to create highly enriched uranium is not an important driver of nuclear war because they believe nuclear deterrence is robust. The crux of Iran’s ability to create highly enriched uranium is therefore a good information crux for one school of thought but not for another school of thought.

To distinguish schools of thought, we need an additional metric: value of discrimination (VoD). VoD measures how well a crux captures the differences between competing models relating to the ultimate question. Formally, for any Party A and Party B who have forecasted P(U) – denoted P_A(U) and P_B(U) – we can evaluate the VoD of any crux (C) as follows:

> Value of Discrimination (VoD) = Initial Disagreement (ID) - Expected Disagreement (ED)

> ID = |P_A(U) - P_B(U)|

> ED = [P_A(U|C) - P_B(U|C)] * [P_A(C) + P_B(C)] / 2 + 
[P_A(U|¬C) - P_B(U|¬C)] * [P_A(¬C) + P_B(¬C)] / 2

Consider a variation on the previous example: Alice and Bob disagree about the chance that two or more countries exchange nuclear strikes in the next decade: Alice believes there is a 5% chance and Bob believes there is a 30% chance. Why do they disagree? As an adherent of the fragile deterrence school of thought, Bob views any of C1, C2, or C3 as events that undermine nuclear deterrence and increase the risk of nuclear war. As an adherent of the robust deterrence school of thought, Alice does not view these events as particularly relevant. They reflect political posturing only; fear of mutually assured destruction would still strongly disincentivize a nuclear first strike. Both Alice and Bob agree, however, that if hypersonic missile technology became widespread, mutually assured destruction would be undermined. Bob thinks that is likely to occur, and Alice thinks that it is highly unlikely to occur. We can evaluate the VoD of the following crux:

- C5: Will two or more countries have hypersonic missiles capable of carrying a nuclear warhead by December 31, 2027?

As shown in the table below, Alice and Bob initially disagree by 25 percentage points (5% vs. 30%). If they both assume that C5 resolves positively, they disagree by only 15 percentage points (25% vs. 40%), and if they both assume that C5 resolves negatively, they disagree by only 17.2 percentage points (2.8% vs. 20%). If we weight those two possible outcomes by the average of Alice’s and Bob’s estimates of their likelihood, we get an *expected disagreement* of 16.6 percentage points. Accordingly, C5 reduced Alice’s and Bob’s disagreement by 8.4 percentage points. In plain language, if one were to ask why Bob and Alice disagree about the probability of nuclear war, one could respond “in part, it’s because they disagree about the likelihood multiple countries will develop hypersonic missiles”. 

|  | P(U) | P(C) | P(U\|C) | P(U\|¬C) | Initial Dis | Exp Dis | VoD |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| Alice | 5% | 10% | 25% | 2.8% | 25% | 16.6% | 8.4% |
| Bob | 30% | 50% | 40% | 20% | 25% | 16.6% | 8.4% |

C5 is a *convergent discrimination* crux, meaning it discriminates between Alice’s and Bob’s models of U by reducing their initial disagreement. Cruxes can also do the opposite, in which case they are *divergent discrimination* cruxes. Divergent discrimination cruxes highlight where people have different models related to P(U) but nonetheless agree on P(U) for different reasons. 

Returning to the above example, imagine Alice and Gerta have the same 5% probability for two countries exchanging nuclear strikes in the next decade. Gerta’s views, however, have little to do with nuclear deterrence. She believes that AGI is right around the corner, and it will render nuclear weapons useless. Alice does not expect AGI to exist any time soon and doesn’t know how, if at all, it would impact the risk of nuclear war. We can evaluate the VoD of the following crux:

- C6: Will AGI exist by December 21, 2027?

As Table xx shows, Alice and Gerta initially agree on P(U). For Alice, C6 has no effect on her P(U) – it’s simply not part of her model. For Gerta, by contrast, C6 matters a great deal. If AGI exists in five years, Gerta thinks the chance of nuclear war goes down to 1%, and if AGI doesn’t exist in five years, Gerta thinks the risk of nuclear war goes up to 21%. Their expected disagreement on P(U) goes from 0% to 3.2%. Accordingly, the VoD of C6 (initial disagreement - expected disagreement) is negative. 

|  | P(U) | P(C) | P(U\|C) | P(U\|¬C) | Initial Dis | Exp Dis | VoD |
|:---:|:---:|:---:|:---:|:---:|:---:|:---:|:---:|
| Alice | 5% | 1% | 5% | 5% | 0% | 3.2% | -3.2% |
| Gerta | 30% | 50% | 40% | 20% | 0% | 3.2% | -3.2% |

As noted  previously, when schools of thought disagree about P(U), we can represent the cleavage points with convergent discrimination cruxes, which have positive VoD. When they agree about P(U) for different reasons, we can represent the cleavage points with divergent discrimination cruxes, which have negative VoD.